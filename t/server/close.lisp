(in-package :cl-user)
(defpackage t-engine-io-server.server.close
  (:use :cl
        :t-engine-io-server.util
        :engine-io-server
        :event-emitter
        :prove
        :ps))
(in-package :t-engine-io-server.server.close)

(plan 29)
(slow-threshold 500)

(diag "- close")

(with-server (:allow-upgrades nil)
  (diag "should be able to access non-empty write-buffer at closing (server)")
  (is (with-waiting
        (on :connection *server*
            (lambda (socket)
              (on :close socket
                  (lambda (reason description)
                    (declare (ignore reason description))
                    (let ((len (length (engine-io-server.socket:write-buffer socket))))
                      (as:with-delay (0.01)
                        (done (list len
                                    (length (engine-io-server.socket:write-buffer socket))))))))
              (vector-push-extend (engine-io-parser:make-packet :type :message
                                                                :data "foo")
                                  (engine-io-server.socket:write-buffer socket))))
        (node-exec
         (with-client-socket (socket)
           (on :open socket
               (lambda ()
                 (close-client-socket socket)
                 (set-timeout exit 100))))))
      '(1 0)))

(with-server (:allow-upgrades nil)
  (diag "should be able to access non-empty writeBuffer at closing (client)")
  (is (node-exec
       (with-client-socket (socket)
         (on :open socket
             (lambda ()
               ((@ socket write-buffer push)
                (create :type "message" :data "foo"))
               ((@ socket callback-buffer push) (lambda ()))
               (on :close socket
                   (lambda (reason)
                     (console.log
                      (+ (@ socket write-buffer length) ":"
                         (@ socket callback-buffer length)))
                     (set-timeout
                      (lambda ()
                        (console.log
                         (+ (@ socket write-buffer length) ":"
                            (@ socket callback-buffer length)))
                        (exit))
                      10)))
               (close-client-socket socket)))))
      "1:1
0:0"))

(with-server (:allow-upgrades nil :ping-interval 5 :ping-timeout 5)
  (diag "should trigger on server if the client does not pong")
  (let (process)
    (is (with-waiting
          (on :connection *server*
              (lambda (socket)
                (on :close socket
                    (lambda (reason description)
                      (declare (ignore description))
                      (done reason)))))
          (setq process
                (node-start
                 (with-client-socket (socket)
                   (setf (@ socket send-packet) (lambda ()))))))
        "ping timeout")
    (external-program:signal-process process :interrupt)))

(with-server (:allow-upgrades nil :ping-interval 500 :ping-timeout 500)
  (diag "should trigger on server even when there is no outstanding polling request (GH-198)")
  (let (process)
    (is (with-waiting
          (on :connection *server*
              (lambda (socket)
                (on :close socket
                    (lambda (reason description)
                      (declare (ignore description))
                      (done reason)))
                (as:with-delay (1)
                  (close-socket socket))))
          (setq process
                (node-start (with-client-socket (socket)
                              (on :open socket
                                  (lambda ()
                                    (setf (@ socket send-packet) (lambda ()))
                                    (setf (@ socket on-packet) (lambda ()))))))))
        "ping timeout")
    (external-program:signal-process process :interrupt)))

(with-server (:allow-upgrades nil :ping-interval 50 :ping-timeout 30)
  (diag "should trigger on client if server does not meet ping timeout")
  (is (node-exec
       (with-client-socket (socket)
         (on :open socket
             (lambda ()
               ;; override onPacket to simulate an inactive server after handshake
               (setf (@ socket on-packet) (lambda ()))
               (on :close socket
                   (lambda (reason err)
                     (console.log reason)
                     (exit)))))))
      "ping timeout"))

(with-server (:allow-upgrades nil :ping-timeout 10 :ping-interval 10)
  (diag "should trigger on both ends upon ping timeout")
  (is (with-waiting
        (on :connection *server*
            (lambda (socket)
              (on :close socket
                  (lambda (reason err)
                    (declare (ignore err))
                    (done reason)))))
        (is (node-exec
             (with-client-socket (socket)
               ((@ socket on) "open"
                (lambda ()
                  (setf (@ socket on-packet) (lambda ()))
                  (setf (@ socket send-packet) (lambda ()))
                  ((@ socket on) "close"
                   (lambda (reason)
                     ((@ console log) reason)
                     ((@ process exit))))))))
            "ping timeout"
            "client"))
      "ping timeout"
      "server"))

(with-server (:allow-upgrades nil)
  (diag "should trigger when server closes a client")
  (is (with-waiting
        (on :connection *server*
            (lambda (socket)
              (on :close socket
                  (lambda (reason err)
                    (declare (ignore err))
                    (done reason)))
              (as:with-delay (0.1)
                (close-socket socket))))
        (is (node-exec
             (with-client-socket (socket)
               (on :open socket
                   (lambda ()
                     (on :close socket
                         (lambda (reason)
                           (console.log reason)
                           (exit)))))))
            "transport close"
            "client"))
      "forced close"
      "server"))

(with-server (:allow-upgrades nil :transports '("websocket"))
  (diag "should trigger when server closes a client (ws)")
  (is (with-waiting
        (on :connection *server*
            (lambda (socket)
              (on :close socket
                  (lambda (reason err)
                    (declare (ignore err))
                    (done reason)))
              (as:with-delay (0.1)
                (close-socket socket))))
        (is (node-exec
             (with-client-socket (socket (ws-localhost) :transports '("websocket"))
               (on :open socket
                   (lambda ()
                     (on :close socket
                         (lambda (reason)
                           (console.log reason)
                           (exit)))))))
            "transport close"
            "client"))
      "forced close"
      "server"))

(with-server (:allow-upgrades nil)
  (diag "should trigger when client closes")
  (is (with-waiting
        (on :connection *server*
            (lambda (socket)
              (on :close socket
                  (lambda (reason err)
                    (declare (ignore err))
                    (done reason)))))
        (is (node-exec
             (with-client-socket (socket)
               (on :open socket
                   (lambda ()
                     (on :close socket
                         (lambda (reason)
                           (console.log reason)
                           (set-timeout exit 10)))
                     (set-timeout (lambda () (close-client-socket socket)) 10)))))
            "forced close"
            "client"))
      "transport close"
      "server"))

(with-server (:allow-upgrades nil :transports '("websocket"))
  (diag "should trigger when client closes (ws)")
  (is (with-waiting
        (on :connection *server*
            (lambda (socket)
              (on :close socket
                  (lambda (reason err)
                    (declare (ignore err))
                    (done reason)))))
        (is (node-exec
             (with-client-socket (socket nil :transports '("websocket"))
               ((@ socket on) "open"
                (lambda ()
                  ((@ socket on) "close"
                   (lambda (reason)
                     ((@ console log) reason)
                     ((@ process exit))))
                  (set-timeout (lambda () ((@ socket close))) 10)))))
            "forced close"
            "client"))
      "transport close"
      "server"))

(with-server (:allow-upgrades nil)
  (diag "should trigger when calling close-socket in payload")
  (is (with-waiting
        (on :connection *server*
            (lambda (socket)
              (on :close socket
                  (lambda (reason err)
                    (declare (ignore err))
                    (done reason)))
              (send socket nil)
              (send socket "this should not be handled")))
        (is (search
             "this should not be handled"
             (node-exec
              (with-client-socket (socket (ws-localhost))
                (let ((closing nil))
                  (on :open socket
                      (lambda ()
                        (on :message socket
                            (lambda (msg)
                              (unless closing
                                (close-client-socket socket))
                              (console.log msg)))
                        (on :close socket
                            (lambda (reason)
                              (console.log reason)
                              (set-timeout exit 300)))))))))
            nil
            "client"))
      "transport close"
      "server"))

(with-server (:allow-upgrades t)
  (diag "should abort upgrade if socket is closed (GH-35)")
  (ok (node-exec
       (with-client-socket (socket)
         ((@ socket on) "open"
          (lambda ()
            ((@ socket close))
            (set-timeout
             (lambda () ((@ process exit))) 100)))))))

#+todo
(with-server (:allow-upgrades nil)
  (diag "should trigger if a poll request is ongoing and the underlying socket closes, as in a browser tab close")
  ;; TODO
  )

(with-server (:allow-upgrades nil)
  (diag "should not trigger with connection: close header")
  (is (with-waiting
        (on :connection *server*
            (lambda (socket)
              (on :message socket
                  (lambda (msg)
                    (send socket "woot")
                    (done msg)))))

        (is (node-exec
             (with-client-socket (socket)
               (on :open socket
                   (lambda () ((@ socket send) "test")))
               (on :message socket
                   (lambda (msg)
                     (console.log msg)
                     (exit)))))
            "woot"
            "client"))
      "test"
      "server"))

(with-server (:allow-upgrades nil :ping-interval 300 :ping-timeout 100)
  (diag "should not trigger early with connection 'ping timeout' after post handshake timeout")
  (is (node-exec
       (with-client-socket (socket)
         (on :handshake socket
             (lambda ()
               (setf (@ socket on-packet) (lambda ()))))
         (on :open socket
             (lambda ()
               (on :close socket
                   (lambda (reason)
                     (console.log reason)))))
         (set-timeout exit 200)))
      ""))

(with-server (:allow-upgrades nil :ping-interval 80 :ping-timeout 50)
  (diag "should not trigger early with connection 'ping timeout' after post ping timeout")
  (let (method)
    (on :connection *server*
        (lambda (socket)
          (on :heartbeat socket
              (lambda ()
                (setf method
                      (defmethod engine-io-server.socket:on-packet ((socket (eql socket)) (packet t))
                        nil))))))
    (is (node-exec
         (with-client-socket (socket)
           (on :open socket
               (lambda ()
                 (on :close socket
                     (lambda (reason)
                       (console.log reason)))))
           (set-timeout exit 100)))
        "")

    (when method
      (remove-method #'engine-io-server.socket:on-packet method))))

(with-server (:allow-upgrades nil :ping-interval 80 :ping-timeout 50)
  (diag "should trigger early with connection 'transport close' after missing pong")
  (on :connection *server*
      (lambda (socket)
        (on :heartbeat socket
            (lambda ()
              (as:with-delay (0.02)
                (close-socket socket))))))
  (is (node-exec
       (with-client-socket (socket)
         (on :open socket
             (lambda ()
               (on :close socket
                   (lambda (reason)
                     (console.log reason)
                     (exit)))))))
      "transport close"))

(with-server (:allow-upgrades nil :ping-interval 300 :ping-timeout 100)
  (diag "should trigger with connection 'ping timeout' after 'ping-interval' + 'ping-timeout'")
  (let (method)
    (on :connection *server*
        (lambda (socket)
          (once :heartbeat socket
                (lambda ()
                  (as:with-delay (0.15)
                    (setf method
                          (defmethod engine-io-server.socket:on-packet ((socket (eql socket)) (packet t))
                            nil)))))))
    (is (node-exec
         (with-client-socket (socket)
           (on :open socket
               (lambda ()
                 (on :close socket
                     (lambda (reason)
                       (console.log reason)))
                 (set-timeout exit 150)))))
        "")
    (is (node-exec
         (with-client-socket (socket)
           (on :open socket
               (lambda ()
                 (on :close socket
                     (lambda (reason)
                       (console.log reason)))
                 (set-timeout exit 350)))))
        "")
    (is (node-exec
         (with-client-socket (socket)
           (on :open socket
               (lambda ()
                 (on :close socket
                     (lambda (reason)
                       (console.log reason)))
                 (set-timeout exit 800)))))
        "ping timeout")
    (when method
      (remove-method #'engine-io-server.socket:on-packet method))))

(with-server (:transports '("websocket"))
  (diag "should trigger transport close before open for ws")
  (is (node-exec
       (with-client-socket (socket "ws://invalidserver:8080")
         (on :open socket abort)
         (on :close socket
             (lambda (reason)
               (console.log reason)
               (exit)))))
      "transport error"))

(with-server (:transports '("polling"))
  (diag "should trigger transport close before open for xhr")
  (is (node-exec
       (with-client-socket (socket "http://invalidserver:8080")
         (on :open socket abort)
         (on :close socket
             (lambda (reason)
               (console.log reason)
               (exit)))))
      "transport error"))

(with-server (:transports '("polling"))
  (diag "should trigger force close before open for xhr")
  (is (node-exec
       (with-client-socket (socket (localhost))
         (on :open socket abort)
         (on :close socket
             (lambda (reason)
               (console.log reason)
               (exit)))
         (close-client-socket socket)))
      "forced close"))

(finalize)
