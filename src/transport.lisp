(in-package :cl-user)
(defpackage engine-io-server.transport
  (:use :cl
        :annot.class)
  (:import-from :engine-io-server.request
                :query-parameter)
  (:import-from :engine-io-server.error
                :transport-error)
  (:import-from :engine-io-parser
                :decode-packet
                :decoding-error)
  (:import-from :event-emitter
                :event-emitter
                :emit
                :listener-count)
  (:import-from :alexandria
                :delete-from-plist))
(in-package :engine-io-server.transport)

(syntax:use-syntax :annot)

@export
@export-accessors
(defclass transport (event-emitter)
  ((sid :accessor sid)
   (transport-ready-state :initarg :ready-state
                          :initform :opening
                          :accessor transport-ready-state)
   (request :initarg :request
            :initform nil
            :accessor request)
   (response :initform nil
             :accessor response)
   (supports-binary :initarg :supports-binary
                    :initform nil
                    :accessor supports-binary)
   (supports-framing :initarg :supports-framing
                     :initform nil
                     :accessor supports-framing)
   (writable :initform nil
             :accessor writable)))


;; event
;; - error
;; - packet
;; - drain
;; - close

(defmethod initialize-instance :after ((transport transport) &rest initargs &key request &allow-other-keys)
  (declare (ignore initargs))
  (when request
    (setf (supports-binary transport)
          (null (query-parameter request "b64")))))

@export
(defun make-transport (name &rest initargs &key request &allow-other-keys)
  (flet ((name-to-class (name request)
           (cond
             ((string= name "websocket")
              (intern #.(string :websocket) :engine-io-server.transport.websocket))
             ((string= name "polling")
              (if (string= (query-parameter request "j")
                           "string")
                  (intern #.(string :polling-jsonp)
                          :engine-io-server.transport.polling-jsonp)
                  (intern #.(string :polling-xhr)
                          :engine-io-server.transport.polling-xhr)))
             (T (error "Unknown transport: ~S" name)))))
    (apply #'make-instance
           (name-to-class name request)
           initargs)))

@export
(defgeneric transport-name (transport)
  (:method ((transport transport))
    (string-downcase (class-name (class-of transport)))))

@export
(defgeneric on-request (transport req res)
  (:method ((transport transport) req res)
    (declare (ignore res))
    (log:debug "setting request")
    (setf (request transport) req)))

@export
(defgeneric send-packets (transport packets))

@export
(defgeneric write-data (transport data))

@export
(defgeneric upgrades-to (transport)
  (:method ((transport transport))
    '()))

@export
(defgeneric headers (transport)
  (:method ((transport transport))
    '()))

@export
(defgeneric close-transport (transport &optional callback)
  (:method ((transport transport) &optional callback)
    (declare (ignore callback))
    nil)
  (:method :before ((transport transport) &optional callback)
    (declare (ignore callback))
    (setf (transport-ready-state transport) :closing)))

(defgeneric on-error (transport desc)
  (:method ((transport transport) desc)
    (when (zerop (listener-count transport :error))
      (log:debug "ignored transport error ~S" desc)
      (return-from on-error))

    (emit :error transport
          (make-instance 'transport-error
                         :description desc))))

(defgeneric on-packet (transport packet)
  (:method ((transport transport) packet)
    (emit :packet transport packet)))

@export
(defgeneric on-data (transport data)
  (:method ((transport transport) data)
    (let ((packet (handler-case (decode-packet data)
                    (decoding-error ()
                      (return-from on-data)))))
      (on-packet transport packet))))

@export
(defgeneric on-close (transport)
  (:method ((transport transport))
    (setf (transport-ready-state transport) :closed)
    (emit :close transport)))
