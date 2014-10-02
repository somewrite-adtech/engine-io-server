(in-package :cl-user)
(defpackage t-engine-io-server.server.packet
  (:use :cl
        :t-engine-io-server.util
        :engine-io-server
        :event-emitter
        :prove
        :ps))
(in-package :t-engine-io-server.server.packet)

(plan 5)

(diag "- packet")

(with-server (:allow-upgrades nil)
  (diag "should emit when socket receives packet")
  (let ((packet (with-waiting
                  (on :connection *server*
                      (lambda (socket)
                        (on :packet socket
                            (lambda (packet)
                              (done packet)))))
                  (node-exec
                   (with-client-socket (socket (ws-localhost))
                     (on :open socket
                         (lambda ()
                           ((@ socket send) "a")
                           (set-timeout exit 300))))))))
    (is-type packet 'packet)
    (is (packet-type packet) :message)
    (is (packet-data packet) "a")))

(with-server (:allow-upgrades nil :ping-interval 4)
  (diag "should emit when receives ping")
  (let ((packet (with-waiting
                  (on :connection *server*
                      (lambda (socket)
                        (on :packet socket
                            (lambda (packet)
                              (close-socket socket)
                              (done packet)))))
                  (node-exec
                   (with-client-socket (socket (ws-localhost))
                     (set-timeout exit 100))))))
    (is-type packet 'packet)
    (is (packet-type packet) :ping)))

(finalize)
