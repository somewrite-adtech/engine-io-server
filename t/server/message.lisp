(in-package :cl-user)
(defpackage t-engine-io-server.server.message
  (:use :cl
        :t-engine-io-server.util
        :engine-io-server
        :event-emitter
        :prove
        :ps))
(in-package :t-engine-io-server.server.message)

(plan 13)
(slow-threshold 500)

(diag "- message")

(with-server (:allow-upgrades nil)
  (diag "should arrive from server to client")
  (on :connection *server*
      (lambda (socket)
        (send socket "a")))
  (is (node-exec
       (with-client-socket (socket)
         (on :open socket
             (lambda ()
               (on :message socket
                   (lambda (msg)
                     (console.log msg)
                     (exit)))))))
      "a"))

(with-server (:allow-upgrades nil)
  (diag "should arrive from server to client (multiple)")
  (on :connection *server*
      (lambda (socket)
        (send socket "a")
        (as:with-delay (0.05)
          (send socket "b")
          (as:with-delay (0.05)
            (send socket "c")
            (close-socket socket)))))
  (is (node-exec
       (with-client-socket (socket)
         (on :open socket
             (lambda ()
               (on :message socket
                   (lambda (msg)
                     ((@ console log) msg)))
               (set-timeout (lambda () ((@ process exit)))
                            300)))))
      "a
b
c"))

(with-server (:allow-upgrades nil :transports '("polling") :max-http-buffer-size 5)
  (diag "should not be receiving data when getting a message longer than maxHttpBufferSize when polling")
  (on :connection *server*
      (lambda (socket)
        (on :message socket
            (lambda (msg)
              (declare (ignore msg))
              (error "Message received unexpectedly")))))
  (node-exec
   (with-client-socket (socket)
     (on :open socket
         (lambda ()
           ((@ socket send) "aasdasdakjhasdkjhasdkjhasdkjhasdkjhasdkjhasdkjha")
           (set-timeout exit 100)))))
  (sleep 1)
  (pass "pass"))

(with-server (:allow-upgrades nil :transports '("polling") :max-http-buffer-size 5)
  (diag "should receive data when getting a message shourter than maxHttpBufferSize when polling")
  (is (with-waiting
        (on :connection *server*
            (lambda (socket)
              (on :message socket
                  (lambda (msg)
                    (done msg)))))
        (node-exec (with-client-socket (socket)
                     (on :open socket
                         (lambda ()
                           ((@ socket send) "a")
                           (set-timeout exit 100))))))
      "a"))

(with-server (:allow-upgrades nil :transports '("websocket"))
  (diag "should arrive from server to client (ws)")
  (on :connection *server*
      (lambda (socket)
        (send socket "a")))
  (is (node-exec (with-client-socket (socket nil :transports '("websocket"))
                   (on :open socket
                       (lambda ()
                         (on :message socket
                             (lambda (msg)
                               (console.log msg)
                               (set-timeout exit 100)))))))
      "a"))

(with-server (:allow-upgrades nil :transports '("websocket"))
  (diag "should arrive from server to client (ws)")
  (on :connection *server*
      (lambda (socket)
        (send socket "a")
        (as:with-delay (0.05)
          (send socket "b")
          (as:with-delay (0.05)
            (send socket "c")
            (close-socket socket)))))
  (is (node-exec
       (with-client-socket (socket nil :transports '("websocket"))
         (on :open socket
             (lambda ()
               (on :message socket
                   (lambda (msg)
                     (console.log msg)))
               (set-timeout exit 300)))))
      "a
b
c"))

(with-server (:allow-upgrades nil :transports '("websocket"))
  (diag "should arrive when binary data is sent as (unsigned-byte 8) vector (ws)")
  (on :connection *server*
      (lambda (socket)
        (send-data socket :message
                   (make-array 5
                               :element-type '(unsigned-byte 8)
                               :initial-contents '(0 1 2 3 4)))))
  (is (node-exec
       (with-client-socket (socket (ws-localhost) :transports '("websocket"))
         (on :open socket
             (lambda ()
               (on :message socket
                   (lambda (msg)
                     (dotimes (i 5)
                       (console.log ((@ msg read-int8) i)))
                     (set-timeout exit 300)))))))
      "0
1
2
3
4"))

(with-server (:allow-upgrades nil :transports '("websocket"))
  (diag "should arrive when binary data is sent as (unsigned-byte 32) vector (ws)")
  (on :connection *server*
      (lambda (socket)
        (send socket (make-array 5
                                 :element-type '(unsigned-byte 32)
                                 :initial-contents (loop for i from 0 to 4
                                                         collect (* (+ i 100) 9823))))))
  (is (node-exec
       (with-client-socket (socket nil :transports '("websocket"))
         (on :open socket
             (lambda ()
               (on :message socket
                   (lambda (msg)
                     (dotimes (i 5)
                       (console.log ((@ msg read-int32-l-e) (* i 4))))
                     (set-timeout exit 300)))))))
      "982300
992123
1001946
1011769
1021592"))

(with-server (:allow-upgrades nil :transports '("polling"))
  (diag "should arrive whe nbinary data sent as (unsigned-byte 8) vector (polling)")
  (on :connection *server*
      (lambda (socket)
        (send-data socket :message
                   (make-array 5
                               :element-type '(unsigned-byte 8)
                               :initial-contents '(0 1 2 3 4)))))
  (is (node-exec
       (with-client-socket (socket (ws-localhost) :transports '("polling"))
         (on :open socket
             (lambda ()
               (on :message socket
                   (lambda (msg)
                     (dotimes (i 5)
                       (console.log ((@ msg read-int8) i)))
                     (set-timeout exit 300)))))))
      "0
1
2
3
4"))

(with-server (:allow-upgrades nil :transports '("polling"))
  (diag "should arrive when binary data is sent as (unsigned-byte 32) vector (polling)")
  (on :connection *server*
      (lambda (socket)
        (send socket (make-array 5
                                 :element-type '(unsigned-byte 32)
                                 :initial-contents (loop for i from 0 to 4
                                                         collect (* (+ i 100) 9823))))))
  (is (node-exec
       (with-client-socket (socket nil :transports '("polling"))
         (on :open socket
             (lambda ()
               (on :message socket
                   (lambda (msg)
                     (dotimes (i 5)
                       (console.log ((@ msg read-int32-l-e) (* i 4))))
                     (set-timeout exit 300)))))))
      "982300
992123
1001946
1011769
1021592"))

;; FIXME
(with-server (:allow-upgrades nil)
  (diag "should trigger a flush/drain event")
  (let ((total-event 4)
        process
        (server *server*))
    (with-waiting
      (on :connection *server*
          (lambda (socket)
            (on :flush server
                (lambda (sock buf)
                  (assert (eq socket sock))
                  (assert (vectorp buf))
                  (when (zerop (decf total-event))
                    (done))))
            (on :flush socket
                (lambda (buf)
                  (assert (vectorp buf))
                  (when (zerop (decf total-event))
                    (done))))
            (on :drain server
                (lambda (sock)
                  (assert (eq socket sock))
                  (assert (zerop (length (engine-io-server.socket::write-buffer socket))))
                  (when (zerop (decf total-event))
                    (done))))
            (on :drain socket
                (lambda ()
                  (assert (zerop (length (engine-io-server.socket::write-buffer socket))))
                  (when (zerop (decf total-event))
                    (done))))

            (send socket "aaaa")))
      (setf process (node-start
                     (with-client-socket (socket (ws-localhost))))))
    (is total-event 0)
    (external-program:signal-process process :interrupt)))

(with-server (:transports '("websocket") :ping-interval 200 :ping-timeout 100)
  (diag "should interleave with pongs if many messages buffered after connection open")
  (let ((message-payload (with-output-to-string (s)
                           (dotimes (i (* 256 256))
                             (write-char #\a s)))))
    (on :connection *server*
        (lambda (socket)
          (dotimes (i 100)
            (send socket (format nil "~A|message: ~A" message-payload i)))))
    (is (node-exec
         (with-client-socket (socket nil :transports '("websocket"))
           (on :open socket
               (lambda ()
                 (let ((received-count 0))
                   (on :message socket
                       (lambda (msg)
                         (when (= (incf received-count) 100)
                           (console.log "done")
                           (exit)))))))))
        "done")))

(with-server (:allow-upgrades nil)
  (diag "should support chinese")
  (let ((shi "石室詩士施氏，嗜獅，誓食十獅。")
        (shi2 "氏時時適市視獅。"))
    (on :connection *server*
        (lambda (socket)
          (once :message socket
                (lambda (msg0)
                  (assert (string= msg0 "."))
                  (once :message socket
                        (lambda (msg)
                          (assert (string= msg (babel:octets-to-string
                                                (babel:string-to-octets shi :encoding :utf-8)
                                                :encoding :latin1)))
                          (once :message socket
                                (lambda (msg2)
                                  (assert (string= msg2
                                                   (babel:octets-to-string
                                                    (babel:string-to-octets shi2 :encoding :utf-8)
                                                    :encoding :latin1)))))))))
          (send socket ".")
          (send socket shi)
          (send socket shi2)))
    (is (node-exec
         (with-client-socket (socket)
           (on :open socket
               (lambda ()
                 (once :message socket
                       (lambda (msg0)
                         (console.log msg0)
                         (once :message socket
                               (lambda (msg)
                                 (console.log msg)
                                 (once :message socket
                                       (lambda (msg2)
                                         (console.log msg2)
                                         ((@ socket send) ".")
                                         ((@ socket send) (lisp shi))
                                         ((@ socket send) (lisp shi2))
                                         (set-timeout exit 300)))))))))))
        ".
石室詩士施氏，嗜獅，誓食十獅。
氏時時適市視獅。")))

(finalize)
