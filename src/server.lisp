(in-package :cl-user)
(defpackage engine-io-server.server
  (:use :cl
        :annot.class)
  (:import-from :engine-io-server.transport
                :make-transport
                :transport-name
                :on-request)
  (:import-from :engine-io-server.socket
                :make-socket
                :socket-transport
                :socket-id
                :socket-upgraded-p
                :maybe-upgrade
                :open-socket
                :close-socket)
  (:import-from :engine-io-server.request
                :make-request
                :query-parameter
                :env
                :raw-env
                :request-method
                :request-uri
                :request-socket)
  (:import-from :engine-io-server.config
                :make-config
                :max-http-buffer-size
                :available-transports)
  (:import-from :engine-io-server.error
                :server-error
                :unknown-transport
                :unknown-sid
                :bad-handshake-method
                :bad-request
                :server-error-code
                :engine-error-description)
  (:import-from :clack
                :<component>
                :call)
  (:import-from :event-emitter
                :event-emitter
                :on
                :once
                :emit)
  (:import-from :cl-async
                :socket-closed)
  (:import-from :websocket-driver
                :make-server-for-clack
                :start-connection
                :close-connection)
  (:import-from :alexandria
                :if-let
                :delete-from-plist))
(in-package :engine-io-server.server)

(syntax:use-syntax :annot)

@export
@export-accessors
(defclass server (<component> event-emitter)
  ((app :initarg :app
        :initform nil
        :accessor app)
   (config :initarg :config
           :initform (make-config)
           :accessor server-config)
   (path :initarg :path
         :initform "/engine.io/"
         :accessor path)
   (cookie :initarg :cookie
           :initform "io"
           :accessor cookie)
   (clients :initform (make-hash-table :test 'equal)
            :accessor clients)
   (clients-count :initform 0
                  :accessor clients-count)))

(defmethod initialize-instance :around ((server-class server)
                                        &rest initargs
                                        &key
                                          (max-http-buffer-size nil max-http-buffer-size-specified-p)
                                          (transports nil transports-specified-p)
                                          (allow-upgrades nil allow-upgrades-specified-p)
                                          (upgrade-timeout nil upgrade-timeout-specified-p)
                                          (ping-timeout nil ping-timeout-specified-p)
                                          (ping-interval nil ping-interval-specified-p)
                                        &allow-other-keys)
  (when (or max-http-buffer-size-specified-p
            transports-specified-p
            allow-upgrades-specified-p
            upgrade-timeout-specified-p
            ping-timeout-specified-p
            ping-interval-specified-p)
    (setq initargs
          (nconc
           (list :config
                 (apply #'make-config
                        (nconc
                         (and max-http-buffer-size-specified-p
                              (list :max-http-buffer-size max-http-buffer-size))
                         (and transports-specified-p
                              (list :available-transports transports))
                         (and allow-upgrades-specified-p
                              (list :allow-upgrades allow-upgrades))
                         (and upgrade-timeout-specified-p
                              (list :upgrade-timeout upgrade-timeout))
                         (and ping-timeout-specified-p
                              (list :ping-timeout ping-timeout))
                         (and ping-interval-specified-p
                              (list :ping-interval ping-interval)))))
           (delete-from-plist initargs
                              :max-http-buffer-size
                              :transports
                              :allow-upgrades
                              :upgrade-timeout))))
  (let* ((server (apply #'call-next-method server-class initargs))
         (path (path server))
         (len (length path)))
    ;; normalize path (adding a '/' to the last)
    (unless (char= (aref path (1- len))
                   #\/)
      (setf (path server)
            (concatenate 'string path "/")))
    server))

(defun transport-available-p (server transport-name)
  (find transport-name
        (available-transports (server-config server))
        :test #'string=))

(defun verify-request (server req &key upgrade)
  (let ((transport-name (query-parameter req "transport")))
    (unless (transport-available-p server transport-name)
      (log:debug "unknown transport ~S" transport-name)
      (error 'unknown-transport :transport transport-name))

    (if-let (sid (query-parameter req "sid"))
      (progn
        (unless (gethash sid (clients server))
          (error 'unknown-sid))
        (unless (or upgrade
                    (string= (transport-name (socket-transport (gethash sid (clients server))))
                             transport-name))
          (log:debug "bad request: unexpected transport without upgrade")
          (error 'bad-request)))
      ;; handshake is GET only
      (unless (eq (request-method req) :get)
        (error 'bad-handshake-method)))))

(defun engineio-path-p (server req)
  (let ((path (path server))
        (uri (request-uri req)))
    (and (<= (length path) (length uri))
         (zerop (search uri
                        path
                        :end1 (length path))))))

(defmethod call ((server server) env)
  (let ((req (make-request env)))
    (cond
      ((engineio-path-p server req)
       (handler-case (if (gethash "upgrade" (gethash :headers (env req)))
                         (handle-upgrade server req)
                         (handle-request server req))
         (server-error (err)
           (error-message req err))))
      ((app server)
       (call (app server) env))
      (T
       '(404 (:content-type "text/plain") ("Not Found"))))))

@export
(defgeneric handle-request (server req)
  (:method ((server server) req)
    (log:debug "handling ~S http request ~S" (request-method req) (request-uri req))
    (verify-request server req :upgrade nil)

    (lambda (responder)
      (let ((res (lambda (&rest args)
                   (handler-case (apply responder args)
                     ;; Ignore if the socket is already closed
                     (as:socket-closed ())
                     ;; Preventing "The value NIL is not of type SYSTEM-AREA-POINTER from wookie:send-response
                     (type-error ())))))
        (if-let (sid (query-parameter req "sid"))
          (progn
            (log:debug "setting new request for existing client")
            (on-request (socket-transport (gethash sid (clients server))) req res))
          (handler-case
              (handshake server
                         (query-parameter req "transport")
                         req
                         res)
            (server-error (err)
              (funcall res (error-message req err)))))))))

@export
(defgeneric handle-upgrade (server req)
  (:method ((server server) req)
    (verify-request server req :upgrade t)

    (let ((upgrade (gethash "upgrade" (gethash :headers (env req)))))
      (if (and upgrade
               (string-equal upgrade "websocket"))
          (let ((ws (wsd:make-server-for-clack (raw-env req))))
            (lambda (responder)
              (let ((res (lambda (&rest args)
                           (handler-case (apply responder args)
                             ;; Ignore if the socket is already closed
                             (as:socket-closed ())
                             ;; Preventing "The value NIL is not of type SYSTEM-AREA-POINTER from wookie:send-response
                             (type-error ())))))
                (once :open ws
                      (lambda (event)
                        (declare (ignore event))
                        (on-websocket server req res ws))))
              (wsd:start-connection ws)))
          (error 'bad-request)))))

(defgeneric handshake (server transport-name req res)
  (:method ((server server) transport-name req res)
    (let* ((transport
             (handler-case
                 (apply #'make-transport transport-name
                        :request req
                        (if (string= transport-name "polling")
                            (list :max-http-buffer-size (max-http-buffer-size (server-config server)))
                            nil))
               (error () (error 'bad-request))))
           (socket
             (make-socket :request req
                          :server server
                          :transport transport
                          :config (server-config server))))
      (log:debug "handshaking client ~S" (socket-id socket))

      (on-request transport req res)

      (let ((sid (socket-id socket)))
        (setf (gethash sid (clients server))
              socket)
        (incf (clients-count server))

        (once :close socket
              (lambda (reason &optional description)
                (declare (ignore reason description))
                (remhash sid (clients server))
                (decf (clients-count server)))))

      (emit :connection server socket))))

(defgeneric close-server (server)
  (:method ((server server))
    (log:debug "closing all open clients")
    (maphash (lambda (id client)
               (declare (ignore id))
               (close-socket client))
             (clients server))
    server))

(defun error-message (req err)
  (let ((origin (gethash "origin" (gethash :headers (env req)))))
    (list 400
          (if origin
              (list :content-type "application/json"
                    :access-control-allow-credentials "true"
                    :access-control-allow-origin origin)
              (list :content-type "application/json"
                    :access-control-allow-origin "*"))
          (list
           (format nil "{\"code\":~D,\"message\":~S}"
                   (server-error-code err)
                   (engine-error-description err))))))

(defgeneric on-websocket (server req res ws)
  (:method ((server server) req res ws)
    (let* ((sid (query-parameter req "sid"))
           (transport-name (query-parameter req "transport"))
           (client (and sid
                        (gethash sid (clients server)))))
      (setf (request-socket req) ws)
      (cond
        ((null sid)
         (handshake server transport-name req res))
        ((null client)
         (log:debug "upgrade attempt for closed client")
         (wsd:close-connection ws))
        ((socket-upgraded-p client)
         (log:debug "transport had already been upgraded")
         (wsd:close-connection ws))
        (T
         (log:debug "upgrading existing transport")
         (let ((transport
                 (make-transport transport-name
                                 :request req)))
           (maybe-upgrade client transport)))))))
