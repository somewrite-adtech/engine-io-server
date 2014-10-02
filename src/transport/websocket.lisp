(in-package :cl-user)
(defpackage engine-io-server.transport.websocket
  (:use :cl)
  (:import-from :engine-io-server.transport
                :transport
                :request
                :writable
                :on-data
                :on-close
                :on-error
                :close-transport
                :send-packets
                :supports-binary)
  (:import-from :engine-io-server.request
                :request-socket)
  (:import-from :engine-io-parser
                :encode-packet)
  (:import-from :cl-async-future
                :attach)
  (:import-from :websocket-driver
                :send
                :close-connection
                :event-data)
  (:import-from :event-emitter
                :on
                :once
                :emit))
(in-package :engine-io-server.transport.websocket)

(syntax:use-syntax :annot)

@export
(defclass websocket (transport)
  ((ws :accessor ws)
   (writable :initform t
             :reader writable)
   (handles-upgrades :initform t)
   (supports-framing :initform t)))

(defmethod initialize-instance :after ((transport websocket) &key)
  (setf (ws transport)
        (request-socket (request transport)))
  (assert (not (null (ws transport))))
  (on :message (ws transport)
      (lambda (ev) (on-data transport (event-data ev))))
  (once :close (ws transport)
        (lambda (ev)
          (declare (ignore ev))
          (on-close transport)))
  (on :error (ws transport)
      (lambda (err)
        (on-error (ws transport)
                  (princ-to-string err)))))

(defmethod on-data :before ((transport websocket) data)
  (log:debug "received ~S" data))

(defmethod send-packets ((transport websocket) packets)
  (setf (writable transport) nil)
  (map nil
       (lambda (packet)
         (let ((data (encode-packet packet :supports-binary (supports-binary transport))))
           (log:debug "writing ~S" data)
           (let ((future (wsd:send (ws transport) data)))
             (asf:attach future
                         (lambda ()
                           (setf (writable transport) t)
                           (emit :drain transport))))))
       packets)
  (lambda (responder)
    (declare (ignore responder))))

(defmethod close-transport ((transport websocket) &optional callback)
  (log:debug "closing")
  (handler-case
      (wsd:close-connection (ws transport))
    (as:socket-closed ()))
  (and callback (funcall callback)))
