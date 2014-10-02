(in-package :cl-user)
(defpackage engine-io-server.error
  (:use :cl
        :annot.class)
  (:export :engine-error
           :transport-error
           :too-large-body
           :engine-error-description
           :server-error
           :unknown-transport
           :unknown-sid
           :bad-request
           :server-error-code))
(in-package :engine-io-server.error)

(define-condition engine-error (simple-error)
  ((description :initarg :description
                :initform nil
                :accessor engine-error-description)))

(define-condition transport-error (engine-error) ())

(define-condition too-large-body (engine-error) ())

(define-condition server-error (engine-error)
  ((code :initarg :code
         :accessor server-error-code)))

(define-condition unknown-transport (server-error)
  ((transport :initarg :transport)
   (code :initform 0)
   (description :initform "Transport unknown")))

(define-condition unknown-sid (server-error)
  ((sid :initarg :sid)
   (code :initform 1)
   (description :initform "Session ID unknown")))

(define-condition bad-handshake-method (server-error)
  ((code :initform 2)
   (description :initform "Bad handshake method")))

(define-condition bad-request (server-error)
  ((code :initform 3)
   (description :initform "Bad request")))
