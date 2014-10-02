(in-package :cl-user)
(defpackage engine-io-server.transport.polling
  (:use :cl)
  (:import-from :engine-io-server.transport
                :transport
                :transport-name
                :request
                :response
                :writable
                :supports-binary
                :on-request
                :on-error
                :on-packet
                :on-close
                :on-data
                :send-packets
                :close-transport
                :upgrades-to
                :write-data)
  (:import-from :engine-io-server.request
                :request-method
                :read-body)
  (:import-from :engine-io-server.error
                :too-large-body)
  (:import-from :event-emitter
                :on
                :emit)
  (:import-from :engine-io-parser
                :make-packet
                :packet-type
                :encode-payload
                :decode-payload
                :decoding-error)
  (:import-from :cl-async
                :delay))
(in-package :engine-io-server.transport.polling)

(syntax:use-syntax :annot)

@export
(defclass polling (transport)
  ((data-request :initform nil
                 :accessor data-request)
   (data-response :initform nil
                  :accessor data-response)
   (max-http-buffer-size :initarg :max-http-buffer-size
                         :accessor max-http-buffer-size)
   (should-close :initform nil
                 :accessor should-close)))

(defmethod initialize-instance :after ((polling polling) &key)
  (setf (request polling) nil))

(defmethod transport-name ((transport polling))
  "polling")

(defmethod on-request ((polling polling) req res)
  (case (request-method req)
    (:get  (on-poll-request polling req res))
    (:post (on-data-request polling req res))
    (otherwise (funcall res '(400 () (""))))))

(defgeneric on-poll-request (polling req res)
  (:method ((polling polling) req res)
    (when (request polling)
      (log:debug "request overlap")
      (on-error polling "overlap from client")
      (return-from on-poll-request
        (funcall res '(400 () ("")))))

    (log:debug "setting request")
    (setf (request polling) req
          (response polling) res)

    (setf (writable polling) t)
    (emit :drain polling)

    (when (and (writable polling)
               (should-close polling))
      (log:debug "triggering empty send to append close packet")
      (send-packets polling (list (make-packet :type :noop))))))

(defgeneric on-data-request (polling req res)
  (:method ((polling polling) req res)
    (when (data-request polling)
      (on-error polling "data request overlap from client")
      (return-from on-data-request
        (funcall res '(400 () ()))))

    (setf (data-request polling) req
          (data-response polling) res)

    (handler-case
        (let ((data (read-body req
                               :max-size (max-http-buffer-size polling))))
          (when (<= (length data) (max-http-buffer-size polling))
            (as:delay
             (lambda ()
               (on-data polling data)))))
      (too-large-body ()))

    (funcall res
             '(200
               (:content-length 2
                ;; text/html is required instead of text/plain to avoid an
                ;; unwanted download dialog on certain user-agents (GH-43)
                :content-type "text/html")
               ("ok")))

    (setf (data-request polling) nil
          (data-response polling) nil)))

(defmethod send-packets ((polling polling) packets)
  (when (should-close polling)
    (log:debug "appending close packet to payload")
    (setf packets
          (nconc packets (list (make-packet :type :close))))
    (funcall (should-close polling))
    (setf (should-close polling) nil))

  (write-data polling
              (encode-payload packets
                              :supports-binary (supports-binary polling))))

(defmethod write-data :before ((polling polling) data)
  (log:debug "writing ~S" data))

(defmethod write-data :after ((polling polling) data)
  (setf (request polling) nil
        (response polling) nil
        (writable polling) nil))

(defmethod close-transport ((polling polling) &optional (callback (lambda ())))
  (log:debug "closing")

  (when (data-request polling)
    (log:debug "aborting ongoing data request")
    (setf (data-request polling) nil))

  (if (writable polling)
      (progn
        (log:debug "transport writable - closing right away")
        (send-packets polling (list (make-packet :type :close)))
        (and callback (funcall callback)))
      (progn
        (log:debug "transport not writable - buffering orderly close")
        (setf (should-close polling) callback))))

(defmethod upgrades-to ((polling polling))
  '("websocket"))

(defmethod on-data ((polling polling) data)
  (log:debug "receive ~S" data)
  (let ((packets (handler-case (decode-payload data)
                   (decoding-error ()
                     (return-from on-data)))))
    (map nil (lambda (packet)
             (if (eq (packet-type packet) :close)
                 (progn
                   (log:debug "got xhr close packet")
                   (on-close polling))
                 (on-packet polling packet)))
         packets)))
