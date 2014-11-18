(in-package :cl-user)
(defpackage t-engine-io-server.server.send
  (:use :cl
        :t-engine-io-server.util
        :engine-io-server
        :event-emitter
        :prove
        :ps))
(in-package :t-engine-io-server.server.send)

(plan 13)
(slow-threshold 500)

(diag "- send")
(diag "-- write buffer")

(with-server (:allow-upgrades nil)
  (diag "should not empty until 'drain' event (polling)")
  (is (node-exec
       (with-client-socket (socket nil :transports '("polling"))
         (let ((total-events 2))
           (on :open socket
               (lambda ()
                 ((@ socket send) "a")
                 ((@ socket send) "b")
                 (console.log (@ socket write-buffer length))))
           (on :drain (@ socket transport)
               (lambda ()
                 (console.log (= (@ socket write-buffer length)
                                 (decf total-events)))
                 (when (= total-events 0)
                   (exit)))))))
      "2
true
true"))

(with-server (:allow-upgrades nil)
  (diag "should not empty until 'drain' event (websocket)")
  (is (node-exec
       (with-client-socket (socket nil :transports '("websocket"))
         (let ((total-events 2))
           (on :open socket
               (lambda ()
                 ((@ socket send) "a")
                 ((@ socket send) "b")
                 (console.log (@ socket write-buffer length))))
           (on :drain (@ socket transport)
               (lambda ()
                 (console.log (= (@ socket write-buffer length)
                                 (decf total-events)))
                 (when (= total-events 0)
                   (exit)))))))
      "2
true
true"))

(diag "-- callback")

(with-server (:allow-upgrades nil)
  (diag "should execute in order when message sent (client) (polling)")
  (on :connection *server*
      (lambda (socket)
        (on :message socket
            (lambda (msg)
              (send socket msg)))))
  (is (node-exec
       (with-client-socket (socket (ws-localhost) :transports '("polling"))
         (let ((i 0) (j 0))
           (flet ((send-fn ()
                    ((@ socket send) j ((lambda (value) (incf j) (values)) j))))
             (on :open socket
                 (lambda ()
                   (on :message socket
                       (lambda (msg)
                         (if (< (incf i) 3)
                             (progn (console.log (= i j))
                                    (send-fn))
                             (exit))))
                   (send-fn)))))))
      "true
true"))

(with-server (:allow-upgrades nil)
  (diag "should execute in order when message sent (client) (websocket)")
  (on :connection *server*
      (lambda (socket)
        (on :message socket
            (lambda (msg)
              (send socket msg)))))
  (is (node-exec
       (with-client-socket (socket (ws-localhost) :transports '("websocket"))
         (let ((i 0) (j 0))
           (flet ((send-fn ()
                    ((@ socket send) j ((lambda (value) (incf j) (values)) j))))
             (on :open socket
                 (lambda ()
                   (on :message socket
                       (lambda (msg)
                         (if (< (incf i) 3)
                             (progn (console.log (= i j))
                                    (send-fn))
                             (exit))))
                   (send-fn)))))))
      "true
true"))

(with-server (:allow-upgrades nil)
  (diag "should execute in order with payloads (client) (polling)")
  (on :connection *server*
      (lambda (socket)
        (on :message socket
            (lambda (msg)
              (send socket msg)))))

  (like (node-exec
         (with-client-socket (socket (ws-localhost) :transports '("polling"))
           (let ((i 0)
                 (last-cb-fired 0))
             (flet ((cb (value)
                      (console.log (equal value (1+ last-cb-fired)))
                      (setf last-cb-fired value)
                      (when (= value 3)
                        (exit))))
               (on :open socket
                   (lambda ()
                     (on :message socket
                         (lambda (msg)
                           (console.log (equal msg (1+ i)))
                           (incf i)))
                     (once :flush socket
                           (lambda ()
                             ((@ socket send) 2 (lambda () (cb 2)))
                             ((@ socket send) 3 (lambda () (cb 3)))))
                     ((@ socket send) 1 (lambda () (cb 1)))))))))
        "^(?:true\\n?)+$"))

(with-server (:allow-upgrades nil)
  (diag "should execute in order with payloads (client) (websocket)")
  (on :connection *server*
      (lambda (socket)
        (on :message socket
            (lambda (msg)
              (send socket msg)))))

  (like (node-exec
         (with-client-socket (socket (ws-localhost) :transports '("websocket"))
           (let ((i 0)
                 (last-cb-fired 0))
             (flet ((cb (value)
                      (console.log (equal value (1+ last-cb-fired)))
                      (setf last-cb-fired value)
                      (when (= value 3)
                        (exit))))
               (on :open socket
                   (lambda ()
                     (on :message socket
                         (lambda (msg)
                           (console.log (equal msg (1+ i)))
                           (incf i)))
                     (once :flush socket
                           (lambda ()
                             ((@ socket send) 2 (lambda () (cb 2)))
                             ((@ socket send) 3 (lambda () (cb 3)))))
                     ((@ socket send) 1 (lambda () (cb 1)))))))))
        "^(?:true\\n?)+$"))

(with-server (:allow-upgrades nil)
  (diag "should execute when message sent (polling)")
  (let ((i 0)
        client-output)
    (on :connection *server*
        (lambda (socket)
          (send socket "a")
          (incf i)))
    (setf client-output
          (node-exec
           (with-client-socket (socket (ws-localhost) :transports '("polling"))
             (let ((j 0))
               (on :open socket
                   (lambda ()
                     (on :message socket
                         (lambda (msg)
                           (incf j)))
                     (set-timeout (lambda () (console.log j) (exit)) 10)))))))
    (is client-output
        (write-to-string i))))

(with-server (:allow-upgrades nil)
  (diag "should execute when message sent (websocket)")
  (let ((i 0)
        client-output)
    (on :connection *server*
        (lambda (socket)
          (send socket "a")
          (incf i)))
    (setf client-output
          (node-exec
           (with-client-socket (socket (ws-localhost) :transports '("websocket"))
             (let ((j 0))
               (on :open socket
                   (lambda ()
                     (on :message socket
                         (lambda (msg)
                           (incf j)))
                     (set-timeout (lambda () (console.log j) (exit)) 10)))))))
    (is client-output
        (write-to-string i))))

(with-server ()
  (diag "should execute once for each send")
  (on :connection *server*
      (lambda (socket)
        (send socket "a")
        (send socket "b")
        (send socket "c")))

  (is (node-exec
       (with-client-socket (socket (ws-localhost))
         (let ((a 0) (b 0) (c 0) (all 0))
           (on :open socket
               (lambda ()
                 (on :message socket
                     (lambda (msg)
                       (cond
                         ((string= msg "a")
                          (incf a))
                         ((string= msg "b")
                          (incf b))
                         ((string= msg "c")
                          (incf c)))

                       (when (= (incf all) 3)
                         (console.log a)
                         (console.log b)
                         (console.log c)
                         (exit)))))))))
      "1
1
1"))

(with-server ()
  (diag "should execute in multipart packet")
  (let ((i 0)
        client-output)
    (on :connection *server*
        (lambda (socket)
          (send socket "b")
          (incf i)
          (send socket "a")
          (incf i)))
    (setf client-output
          (node-exec
           (with-client-socket (socket (ws-localhost))
             (let ((j 0))
               (on :open socket
                   (lambda ()
                     (on :message socket
                         (lambda (msg)
                           (incf j)))
                     (set-timeout (lambda () (console.log j) (exit)) 200)))))))
    (is client-output
        (write-to-string i))))

(with-server ()
  (diag "should execute in multipart packet (polling)")
  (let ((i 0)
        client-output)
    (on :connection *server*
        (lambda (socket)
          (send socket "d")
          (incf i)
          (send socket "c")
          (incf i)
          (send socket "b")
          (incf i)
          (send socket "a")
          (incf i)))
    (setf client-output
          (node-exec
           (with-client-socket (socket (ws-localhost) :transports '("polling"))
             (let ((j 0))
               (on :open socket
                   (lambda ()
                     (on :message socket
                         (lambda (msg)
                           (incf j)))
                     (set-timeout (lambda () (console.log j) (exit)) 200)))))))
    (is client-output
        (write-to-string i))))

(with-server (:allow-upgrades nil)
  (diag "should clean callback references when socket gets closed with pending callbacks")
  (pass "No need to test because this Engine.IO server doesn't have callback feature."))

;; FIXME: This test is meaningless. Fix this when 'send' allows to set a callback function.
(with-server (:allow-upgrades nil)
  (diag "should not execute when it is not actually sent (polling)")
  (is (with-waiting
        (on :connection *server*
            (lambda (socket)
              (on :close socket
                  (lambda (reason description)
                    (declare (ignore reason description))
                    (done)))
              (send socket "a")
              (send socket "b")))

        (node-exec
         (with-client-socket (socket (ws-localhost) :transports '("polling"))
           (on "pollComplete" (@ socket transport)
               (lambda (msg)
                 (close-client-socket socket)
                 (set-timeout exit 300))))))
      nil))

(finalize)
