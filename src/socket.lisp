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
                :event-emitter*
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
(defstruct (socket (:include event-emitter*)
                   (:constructor %make-socket))
  (id (princ-to-string (make-v4-uuid)))
  (server (error ":server is required"))
  (request (error ":request is required"))
  (transport (error ":transport is required"))
  (config (error ":config is required"))
  (ready-state :opening)
  (upgraded-p nil)
  (write-buffer (make-write-buffer))
  (packets-fn '())
  (sent-callback-fn '())
  (timers (make-timers)))

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

@export
(defun make-socket (&rest initargs &key transport &allow-other-keys)
  (let ((socket (apply #'%make-socket initargs)))
    (prepare-transport socket transport)
    (open-socket socket)
    socket))

@export
(defun prepare-transport (socket transport)
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
  (setup-send-callback socket))

@export
(defun open-socket (socket)
  (setf (socket-ready-state socket) :open)
  (setf (sid (socket-transport socket))
        (socket-id socket))

  (let ((config (socket-config socket)))
    (send-data socket :open
               (jsown:to-json
                `(:obj
                  ("sid"          . ,(socket-id socket))
                  ("upgrades"     . ,(available-upgrades socket))
                  ("pingInterval" . ,(ping-interval config))
                  ("pingTimeout"  . ,(ping-timeout config))))))

  (emit :open socket)
  (set-ping-timeout socket))

@export
(defun on-packet (socket packet)
  (unless (eq (socket-ready-state socket) :open)
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
     (emit :message socket (packet-data packet)))))

@export
(defun on-error (socket err)
  (log:debug "transport error")
  (on-close socket "transport error" err))

@export
(defun on-close (socket reason &optional description)
  (unless (eq (socket-ready-state socket) :closed)
    (stop-timer (socket-timers socket) :ping-timeout)
    (stop-timer (socket-timers socket) :check-interval)
    (stop-timer (socket-timers socket) :upgrade-timeout)
    (setf (socket-ready-state socket) :closed)
    (emit :close socket reason description)
    (setf (socket-write-buffer socket)
          (make-write-buffer))
    (setf (slot-value socket 'packets-fn) '())
    (setf (slot-value socket 'sent-callback-fn) '())
    (values)))

(defun start-check-interval (socket)
  (flet ((send-noop ()
           (let ((transport (socket-transport socket)))
             (when (and (string= (transport-name transport) "polling")
                        (writable transport))
               (log:debug "writing a noop packet to polling for fast upgrade")
               (send-packets transport
                             (list (make-packet :type :noop)))))))

    (start-timer (socket-timers socket) :check-interval
                 #'send-noop
                 :time 0.1)))

@export
(defun maybe-upgrade (socket transport)
  (let ((current-transport (socket-transport socket)))
    (log:debug "might upgrade socket transport from ~S to ~S"
               (transport-name current-transport)
               (transport-name transport))
    (start-timer (socket-timers socket) :upgrade-timeout
                 (lambda ()
                   (log:debug "client did not complete upgrade - closing transport")
                   (stop-timer (socket-timers socket) :check-interval)
                   (when (eq (transport-ready-state transport) :open)
                     (close-transport transport)))
                                        :time (/ (upgrade-timeout (socket-config socket)) 1000))
    (labels ((on-packet (packet)
               (cond
                 ((and (eq (packet-type packet) :ping)
                       (string= (packet-data packet) "probe"))
                  (send-packets transport
                                (list
                                 (make-packet :type :pong :data "probe")))
                  (start-check-interval socket))
                 ((and (eq (packet-type packet) :upgrade)
                       (eq (socket-ready-state socket) :open))
                  (log:debug "got upgrade packet - upgrading")
                  (setf (socket-upgraded-p socket) t)
                  (on :error transport
                      (lambda (err)
                        (declare (ignore err))
                        (log:debug "error triggered by discarded transport")))
                  (stop-timer (socket-timers socket) :ping-timeout)
                  (setf (socket-transport socket) transport)
                  (prepare-transport socket transport)
                  (emit :upgrade socket transport)
                  (set-ping-timeout socket)
                  (flush-buffer socket)
                  (stop-timer (socket-timers socket) :check-interval)
                  (stop-timer (socket-timers socket) :upgrade-timeout)
                  (remove-listener transport :packet #'on-packet))
                 (T (close-transport transport)))))
      (on :packet transport #'on-packet))))

(defun set-ping-timeout (socket)
  (stop-timer (socket-timers socket) :ping-timeout)
  (start-timer (socket-timers socket) :ping-timeout
               (lambda ()
                 (on-close socket "ping timeout"))
                                      :time
                                      (/ (+ (ping-interval (socket-config socket))
                                            (ping-timeout (socket-config socket)))
                                         1000)))

@export
(defun send (socket data &optional fn)
  (send-data socket :message data fn))

@export
(defun send-data (socket type &optional data fn)
  (when (eq (socket-ready-state socket) :closing)
    (return-from send-data))

  (log:debug "sending packet ~S (~S)" type data)

  (let ((packet (make-packet :type type
                             :data data)))
    (emit :packet-create socket packet)

    (vector-push-extend packet (socket-write-buffer socket))

    (when fn
      (push fn (slot-value socket 'packets-fn)))

    (flush-buffer socket)))

@export
(defun flush-buffer (socket)
  (unless (and (not (eq (socket-ready-state socket) :closed))
               (writable (socket-transport socket))
               (not (zerop (length (socket-write-buffer socket)))))
    (return-from flush-buffer))

  (log:debug "flushing buffer to transport")

  (emit :flush socket (socket-write-buffer socket))
  (emit :flush (socket-server socket) socket (socket-write-buffer socket))

  (let ((wbuf (socket-write-buffer socket)))
    (setf (socket-write-buffer socket)
          (make-write-buffer))
    (if (supports-framing (socket-transport socket))
        (setf (slot-value socket 'sent-callback-fn)
              (append (slot-value socket 'packets-fn)
                      (slot-value socket 'sent-callback-fn)))
        (push (slot-value socket 'packets-fn)
              (slot-value socket 'sent-callback-fn)))
    (setf (slot-value socket 'packets-fn) '())
    (send-packets (socket-transport socket) (coerce wbuf 'list))) ;; XXX

  (emit :drain socket)
  (emit :drain (socket-server socket) socket)

  (values))

(defun setup-send-callback (socket)
  (on :drain (socket-transport socket)
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
(defun upgrades (socket transport)
  (if (allow-upgrades (socket-config socket))
      (upgrades-to transport)
      '()))

@export
(defun available-upgrades (socket)
  (loop for upgrade in (upgrades socket (socket-transport socket))
        when (find upgrade (available-transports (socket-config socket))
                   :test #'string=)
          collect upgrade))

@export
(defun close-socket (socket)
  (when (eq (socket-ready-state socket) :open)
    (setf (socket-ready-state socket) :closing)
    (close-transport (socket-transport socket)
                     (lambda () (on-close socket "forced close")))))
