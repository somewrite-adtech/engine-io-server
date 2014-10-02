(in-package :cl-user)
(defpackage engine-io-server
  (:nicknames :eio-server)
  (:use :cl)
  (:import-from :engine-io-server.server
                :server
                :close-server)
  (:import-from :clack
                :clackup
                :stop))
(in-package :engine-io-server)

(syntax:use-syntax :annot)

(defstruct handler
  clack-handler
  server)

@export
(defun start-server (server &key (handler :wookie) (port 5000) (debug t))
  (check-type server server)
  (let ((clack-handler (clack:clackup server
                                      :server handler
                                      :port port
                                      :debug debug)))
    (make-handler :server server :clack-handler clack-handler)))

@export
(defun stop-server (handler)
  (close-server (handler-server handler))
  (clack:stop (handler-clack-handler handler)))

(cl-reexport:reexport-from :engine-io-server.server
                           :include '(:server
                                      :app
                                      :server-config
                                      :path
                                      :cookie
                                      :clients
                                      :clients-count
                                      :close-server))

(cl-reexport:reexport-from :engine-io-server.socket
                           :include '(:socket
                                      :socket-id
                                      :socket-request
                                      :socket-transport
                                      :socket-config
                                      :socket-ready-state
                                      :socket-upgraded-p
                                      :send
                                      :send-data
                                      :close-socket))

(cl-reexport:reexport-from :engine-io-server.transport
                           :include '(:transport
                                      :transport-name))

(cl-reexport:reexport-from :engine-io-server.request
                           :include '(:env
                                      :raw-env
                                      :request-method
                                      :request-uri
                                      :query-string
                                      :query-parameter
                                      :read-body-as-text
                                      :read-body-as-binary
                                      :read-body))

(cl-reexport:reexport-from :engine-io-server.config
                           :include '(:max-http-buffer-size
                                      :available-transports
                                      :ping-timeout
                                      :ping-interval
                                      :allow-upgrades
                                      :upgrade-timeout))

(cl-reexport:reexport-from :event-emitter
                           :include '(:add-listener
                                      :on
                                      :once
                                      :remove-listener
                                      :remove-all-listeners
                                      :listeners
                                      :emit
                                      :listener-count))

(cl-reexport:reexport-from :engine-io-parser
                           :include '(:packet
                                      :packet-type
                                      :packet-data))
