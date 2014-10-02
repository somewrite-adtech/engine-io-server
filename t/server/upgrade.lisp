(in-package :cl-user)
(defpackage t-engine-io-server.server.upgrade
  (:use :cl
        :t-engine-io-server.util
        :engine-io-server
        :event-emitter
        :prove
        :ps))
(in-package :t-engine-io-server.server.upgrade)

(plan 2)
(slow-threshold 500)

(diag "- upgrade")

(with-server ()
  (diag "should upgrade")
  (with-waiting
    (on :connection *server*
        (lambda (socket)
          (let ((last-sent 0)
                (last-received 0)
                (upgraded nil)
                interval)
            (setf interval
                  (as:with-interval (0.002)
                    (incf last-sent)
                    (send socket last-sent)
                    (when (= last-sent 50)
                      (as:remove-interval interval)
                      (as:with-delay (0.05)
                        (close-socket socket)))))

            (assert (string= (query-parameter (request socket) "transport")
                             "polling"))

            (on :message socket
                (lambda (msg)
                  (incf last-received)
                  (assert (string= msg (write-to-string last-received)))))

            (on :upgrade socket
                (lambda (to)
                  (assert (string= (query-parameter (request socket) "transport")
                                   "polling"))
                  (setf upgraded t)
                  (assert (string= (transport-name to) "websocket"))
                  (assert (string= (transport-name (transport socket)) "websocket"))))

            (on :close socket
                (lambda (reason description)
                  (declare (ignore description))
                  (assert (string= reason "transport close"))
                  (assert (= last-sent 50))
                  (assert (= last-received 50))
                  (assert upgraded)
                  (done))))))
    (like (node-exec
           (with-client-socket (socket (ws-localhost))
             (let ((last-sent 0)
                   (last-received 0)
                   (upgrades 0)
                   interval)
               (on :open socket
                   (lambda ()
                     (setf interval
                           (set-interval
                            (lambda ()
                              ((@ socket send) (incf last-sent))
                              (when (= last-sent 50)
                                (clear-interval interval)
                                (set-timeout
                                 (lambda ()
                                   (close-client-socket socket)
                                   (set-timeout exit 100))
                                 10)))
                            2))
                     (on :upgrading socket
                         (lambda (to)
                           (console.log (= (@ to name) "websocket"))
                           (incf upgrades)
                           ((@ socket send) (incf last-sent))
                           ((@ socket send) (incf last-sent))
                           (console.log (< 0 (@ socket write-buffer length)))))
                     (on :upgrade socket
                         (lambda (to)
                           (console.log (= (@ to name) "websocket"))
                           (incf upgrades)))
                     (on :message socket
                         (lambda (msg)
                           (incf last-received)
                           (console.log (equal last-received msg))))
                     (on :close socket
                         (lambda (reason)
                           (console.log (= reason "forced close"))
                           (console.log (= last-sent 50))
                           (console.log (= last-received 50))
                           (console.log (= upgrades 2))
                           (exit))))))))
          "^(?:true\\n?)+$"))
  (pass "pass"))

(finalize)
