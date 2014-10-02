(in-package :cl-user)
(defpackage engine-io-server.socket
  (:use :cl
        :annot.class)
  (:import-from :engine-io-server.transport
                :transport
                :transport-name
                :sid
                :transport-ready-state
                :supports-framing
                :writable
                :upgrades-to
                :send-packets
                :close-transport)
  (:import-from :engine-io-server.config
                :available-transports
                :ping-timeout
                :ping-interval
                :allow-upgrades
                :upgrade-timeout)
  (:import-from :engine-io-server.timer
                :timers
                :make-timers
                :start-timer
                :stop-timer)
  (:import-from :engine-io-parser
                :packet
                :make-packet
                :packet-type
                :packet-data)
  (:import-from :event-emitter
                :event-emitter
                :on
                :once
                :emit
                :remove-listener)
  (:import-from :cl-async
                :delay
                :remove-event)
  (:import-from :cl-async-future
                :make-future
                :finish)
  (:import-from :jsown
                :to-json)
  (:import-from :unicly
                :make-v4-uuid)
  (:import-from :alexandria
                :when-let))
(in-package :engine-io-server.socket)

(syntax:use-syntax :annot)

@export
@export-accessors
(defclass socket (event-emitter)
  ((id :initarg :id
       :initform (princ-to-string (make-v4-uuid))
       :accessor socket-id)
   (server :initarg :server
           :initform (error ":server is required")
           :accessor server)
   (request :initarg :request
            :initform (error ":request is required")
            :reader request)
   (transport :initarg :transport
              :initform (error ":transport is required")
              :reader transport)
   (config :initarg :config
           :initform (error ":config is required")
           :accessor config)
   (ready-state :initform :opening
                :accessor ready-state)
   (upgradedp :initform nil
              :accessor upgradedp)
   (write-buffer :initform (make-write-buffer)
                 :accessor write-buffer)
   (packets-fn :initform '())
   (sent-callback-fn :initform '())
   (timers :initform (make-timers)
           :accessor timers)))

(defun make-write-buffer ()
  (make-array 0
              :adjustable t
              :fill-pointer 0
              :element-type 'packet))

;; event
;; - connection
;; - close
;; - open
;; - packet
;; - heartbeat
;; - data
;; - message
;; - upgrade

(defmethod initialize-instance :after ((socket socket) &key)
  (setf (transport socket)
        (slot-value socket 'transport))
  (open-socket socket))

@export
(defgeneric (setf transport) (transport socket)
  (:method (transport (socket socket))
    (setf (slot-value socket 'transport) transport)
    (once :error transport
          (lambda (err)
            (on-error socket err)))
    (on :packet transport
        (lambda (packet)
          (on-packet socket packet)))
    (on :drain transport
        (lambda ()
          (flush-buffer socket)))
    (once :close transport
          (lambda ()
            (on-close socket "transport close")))
    (setup-send-callback socket)))

@export
(defgeneric open-socket (socket)
  (:method ((socket socket))
    (setf (ready-state socket) :open)
    (setf (sid (transport socket))
          (socket-id socket))

    (let ((config (config socket)))
      (send-data socket :open
                 (jsown:to-json
                  `(:obj
                    ("sid"          . ,(socket-id socket))
                    ("upgrades"     . ,(available-upgrades socket))
                    ("pingInterval" . ,(ping-interval config))
                    ("pingTimeout"  . ,(ping-timeout config))))))

    (emit :open socket)
    (set-ping-timeout socket)))

@export
(defgeneric on-packet (socket packet)
  (:method ((socket socket) packet)
    (unless (eq (ready-state socket) :open)
      (log:debug "packet received with closed socket")
      (return-from on-packet))

    (log:debug "packet")
    (emit :packet socket packet)

    ;; Reset ping timeout on any packet, incoming data is a good sign of
    ;; other side's liveness
    (set-ping-timeout socket)

    (case (packet-type packet)
      (:ping
       (log:debug "got ping")
       (send-data socket :pong)
       (emit :heartbeat socket))
      (:message
       (emit :data socket (packet-data packet))
       (emit :message socket (packet-data packet))))))

@export
(defgeneric on-error (socket err)
  (:method ((socket socket) err)
    (log:debug "transport error")
    (on-close socket "transport error" err)))

@export
(defgeneric on-close (socket reason &optional description)
  (:method ((socket socket) reason &optional description)
    (unless (eq (ready-state socket) :closed)
      (stop-timer (timers socket) :ping-timeout)
      (stop-timer (timers socket) :check-interval)
      (stop-timer (timers socket) :upgrade-timeout)
      (setf (ready-state socket) :closed)
      (emit :close socket reason description)
      (setf (write-buffer socket)
            (make-write-buffer))
      (setf (slot-value socket 'packets-fn) '())
      (setf (slot-value socket 'sent-callback-fn) '())
      (values))))

(defun start-check-interval (socket)
  (flet ((send-noop ()
           (let ((transport (transport socket)))
             (when (and (string= (transport-name transport) "polling")
                        (writable transport))
               (log:debug "writing a noop packet to polling for fast upgrade")
               (send-packets transport
                             (list (make-packet :type :noop)))))))

    (start-timer (timers socket) :check-interval
                 #'send-noop
                 :time 0.1)))

@export
(defgeneric maybe-upgrade (socket transport)
  (:method ((socket socket) transport)
    (let ((current-transport (transport socket)))
      (log:debug "might upgrade socket transport from ~S to ~S"
                 (transport-name current-transport)
                 (transport-name transport))
      (start-timer (timers socket) :upgrade-timeout
                   (lambda ()
                     (log:debug "client did not complete upgrade - closing transport")
                     (stop-timer (timers socket) :check-interval)
                     (when (eq (transport-ready-state transport) :open)
                       (close-transport transport)))
                   :time (/ (upgrade-timeout (config socket)) 1000))
      (labels ((on-packet (packet)
                 (cond
                   ((and (eq (packet-type packet) :ping)
                         (string= (packet-data packet) "probe"))
                    (send-packets transport
                                  (list
                                   (make-packet :type :pong :data "probe")))
                    (start-check-interval socket))
                   ((and (eq (packet-type packet) :upgrade)
                         (eq (ready-state socket) :open))
                    (log:debug "got upgrade packet - upgrading")
                    (setf (upgradedp socket) t)
                    (on :error transport
                        (lambda (err)
                          (declare (ignore err))
                          (log:debug "error triggered by discarded transport")))
                    (stop-timer (timers socket) :ping-timeout)
                    (setf (transport socket) transport)
                    (emit :upgrade socket transport)
                    (set-ping-timeout socket)
                    (flush-buffer socket)
                    (stop-timer (timers socket) :check-interval)
                    (stop-timer (timers socket) :upgrade-timeout)
                    (remove-listener transport :packet #'on-packet))
                   (T (close-transport transport)))))
        (on :packet transport #'on-packet)))))

(defgeneric set-ping-timeout (socket)
  (:method ((socket socket))
    (stop-timer (timers socket) :ping-timeout)
    (start-timer (timers socket) :ping-timeout
                 (lambda ()
                   (on-close socket "ping timeout"))
                 :time
                 (/ (+ (ping-interval (config socket))
                       (ping-timeout (config socket)))
                    1000))))

@export
(defun send (socket data &optional fn)
  (send-data socket :message data fn))

@export
(defgeneric send-data (socket type &optional data fn)
  (:method ((socket socket) type &optional data fn)
    (when (eq (ready-state socket) :closing)
      (return-from send-data))

    (log:debug "sending packet ~S (~S)" type data)

    (let ((packet (make-packet :type type
                               :data data)))
      (emit :packet-create socket packet)

      (vector-push-extend packet (write-buffer socket))

      (when fn
        (push fn (slot-value socket 'packets-fn)))

      (flush-buffer socket))))

@export
(defgeneric flush-buffer (socket)
  (:method ((socket socket))
    (unless (and (not (eq (ready-state socket) :closed))
                 (writable (transport socket))
                 (not (zerop (length (write-buffer socket)))))
      (return-from flush-buffer))

    (log:debug "flushing buffer to transport")

    (emit :flush socket (write-buffer socket))
    (emit :flush (server socket) socket (write-buffer socket))

    (let ((wbuf (write-buffer socket)))
      (setf (write-buffer socket)
            (make-write-buffer))
      (if (supports-framing (transport socket))
          (setf (slot-value socket 'sent-callback-fn)
                (append (slot-value socket 'packets-fn)
                        (slot-value socket 'sent-callback-fn)))
          (push (slot-value socket 'packets-fn)
                (slot-value socket 'sent-callback-fn)))
      (setf (slot-value socket 'packets-fn) '())
      (send-packets (transport socket) (coerce wbuf 'list))) ;; XXX

    (emit :drain socket)
    (emit :drain (server socket) socket)

    (values)))

(defun setup-send-callback (socket)
  (on :drain (transport socket)
      (lambda ()
        (dolist (fn (nreverse (slot-value socket 'sent-callback-fn)))
          (typecase fn
            (function
             (log:debug "executing send callback")
             (funcall fn))
            (list
             (log:debug "executing batch send callback")
             (dolist (fn (nreverse fn))
               (when (functionp fn)
                 (funcall fn))))))
        (setf (slot-value socket 'sent-callback-fn) '()))))

@export
(defgeneric upgrades (socket transport)
  (:method ((socket socket) transport)
    (if (allow-upgrades (config socket))
        (upgrades-to transport)
        '())))

@export
(defgeneric available-upgrades (socket)
  (:method ((socket socket))
    (loop for upgrade in (upgrades socket (transport socket))
          when (find upgrade (available-transports (config socket))
                     :test #'string=)
            collect upgrade)))

@export
(defgeneric close-socket (socket)
  (:method ((socket socket))
    (when (eq (ready-state socket) :open)
      (setf (ready-state socket) :closing)
      (close-transport (transport socket)
                       (lambda () (on-close socket "forced close"))))))
