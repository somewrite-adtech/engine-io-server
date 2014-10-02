(in-package :cl-user)
(defpackage t-engine-io-server.server.packet-create
  (:use :cl
        :t-engine-io-server.util
        :engine-io-server
        :event-emitter
        :prove
        :ps))
(in-package :t-engine-io-server.server.packet-create)

(plan 5)

(diag "- packet-create")

(with-server (:allow-upgrades nil)
  (let ((packet (with-waiting
                  (on :connection *server*
                      (lambda (socket)
                        (on :packet-create socket
                            (lambda (packet)
                              (done packet)))
                        (send socket "a")))
                  (node-exec (with-client-socket (socket) (set-timeout exit 100))))))
    (is-type packet 'packet)
    (is (packet-type packet) :message)
    (is (packet-data packet) "a")))

(with-server (:allow-upgrades nil :ping-interval 4)
  (diag "should emit before send pong")
  (let ((packet (with-waiting
                  (on :connection *server*
                      (lambda (socket)
                        (on :packet-create socket
                            (lambda (packet)
                              (close-socket socket)
                              (done packet)))))
                  (node-exec (with-client-socket (socket) (set-timeout exit 100))))))
    (is-type packet 'packet)
    (is (packet-type packet) :pong)))

(finalize)
