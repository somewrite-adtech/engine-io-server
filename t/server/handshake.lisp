(in-package :cl-user)
(defpackage t-engine-io-server.server.handshake
  (:use :cl
        :t-engine-io-server.util
        :engine-io-server
        :event-emitter
        :prove
        :ps))
(in-package :t-engine-io-server.server.handshake)

(plan 26)
(slow-threshold 500)

(diag "- handshake")

(with-server ()
  (diag "should send the io cookie")
  (multiple-value-bind (body status headers)
      (http-get (localhost "/engine.io/default/"
                           '(("transport" . "polling")
                             ("b64" . 1))))
    (is status 200)
    (let ((sid (elt (match body "\"sid\":\"([^\"]+)") 0)))
      (is (cdr (assoc :set-cookie headers))
          (format nil "io=~A" sid)))))

(with-server (:cookie "woot")
  (diag "should send the io cookie custom name")
  (multiple-value-bind (body status headers)
      (http-get (localhost "/engine.io/default/"
                           '(("transport" . "polling")
                             ("b64" . 1))))
    (is status 200)
    (let ((sid (elt (match body "\"sid\":\"([^\"]+)") 0)))
      (is (cdr (assoc :set-cookie headers))
          (format nil "woot=~A" sid)))))

(with-server (:cookie nil)
  (diag "should not send the io cookie")
  (multiple-value-bind (body status headers)
      (http-get (localhost "/engine.io/default/"
                           '(("transport" . "polling"))))
    (declare (ignore body))
    (is status 200)
    (is (cdr (assoc :set-cookie headers)) nil)))

(with-server (:allow-upgrades nil)
  (diag "should register a new client")
  (is (hash-table-count (engine-io-server.server:clients *server*)) 0)
  (is (clients-count *server*) 0)

  (let (process)
    (with-waiting
      (on :connection *server*
          (lambda (socket)
            (declare (ignore socket))
            (done)))
      (setq process (node-start (with-client-socket (socket)))))
    (is (hash-table-count (engine-io-server.server:clients *server*)) 1)
    (is (clients-count *server*) 1)
    (external-program:signal-process process :interrupt)))

(with-server (:allow-upgrades nil)
  (diag "should exchange handshake data")
  (like (node-exec
         (with-client-socket (socket)
           ((@ socket on) "handshake"
            (lambda (obj)
              ((@ console log)
               (+ (= (typeof (@ obj sid)) "string") ":"
                  (= (typeof (@ obj ping-timeout)) "number") ":"
                  ((@ -array is-array) (@ obj upgrades))
                  " / "
                  (@ obj sid) ":"
                  (@ obj ping-timeout) ":"
                  (@ obj upgrades)))
              ((@ process exit))))
           (set-timeout (lambda ()) (* 30 1000))))
        "^true:true:true"))

(with-server (:allow-upgrades nil :ping-timeout 123)
  (diag "should allow custom ping timeouts")
  (is (node-exec
       (with-client-socket (socket (localhost))
         ((@ socket on) "handshake"
          (lambda (obj)
            ((@ console log) (@ obj ping-timeout))
            ((@ process exit))))))
      "123"))

(with-server (:allow-upgrades nil)
  (diag "should trigger a connection event with a Socket")
  (is (with-waiting
        (on :connection *server*
            (lambda (socket)
              (done (type-of socket))))
        (node-exec
         (with-client-socket (socket)
           (set-timeout (lambda () ((@ process exit))) 500))))
      'socket))

(with-server (:allow-upgrades nil)
  (diag "should open with polling by default")
  (like (with-waiting
          (on :connection *server*
              (lambda (socket)
                (done (transport-name (socket-transport socket)))))
          (node-exec
           (with-client-socket (socket)
             ((@ socket on) "open"
              (lambda ()
                (set-timeout ((@ process exit)) 500))))))
        "^polling"))

(with-server (:transports '("websocket"))
  (diag "should be able to open with ws directly")
  (is (with-waiting
        (on :connection *server*
            (lambda (socket)
              (done (transport-name (socket-transport socket)))))
        (node-exec
         (with-client-socket (socket nil :transports '("websocket"))
           (set-timeout (lambda () ((@ process exit))) 500))))
      "websocket"))

(with-server (:transports '("websocket"))
  (diag "should not suggest any upgrades for websocket")
  (is (node-exec
       (with-client-socket (socket nil :transports '("websocket"))
         ((@ socket on) "handshake"
          (lambda (obj)
            ((@ console log) (@ obj upgrades length))
            ((@ process exit))))))
      "0"))

(with-server (:transports '("polling"))
  (diag "should not suggest upgrades when none are available")
  (is (node-exec
       (with-client-socket (socket nil '())
         ((@ socket on) "handshake"
          (lambda (obj)
            ((@ console log) (@ obj upgrades length))
            ((@ process exit))))))
      "0"))

(with-server ()
  (diag "should suggest all upgrades when no transports are disabled")
  (is (node-exec
       (with-client-socket (socket nil '())
         ((@ socket on) "handshake"
          (lambda (obj)
            ((@ console log)
             (+ (@ obj upgrades length) ":"
                (<= 0 ((@ obj upgrades index-of) "websocket"))))
            (close-client-socket socket)
            ((@ process exit))))
         (set-timeout (lambda () ((@ process abort))) (* 30 1000))))
      "1:true"))

(with-server (:allow-upgrades nil)
  (diag "default to polling when proxy doesn't support websocket")
  (on :connection *server*
      (lambda (socket)
        (on :message socket
            (lambda (msg)
              (when (string= msg "echo")
                (send-data socket :message msg))))))
  (is (node-exec
       (with-client-socket (socket)
         (let ((request (require "superagent")))
           ((@ socket on) "open"
            (lambda ()
              (chain request
                     (get (lisp (localhost "/engine.io/")))
                     (set (create :connection "close"))
                     (query (create :transport "websocket" :sid (@ socket id)))
                     (end (lambda (err res)
                            ((@ console log)
                             (+ err ":"
                                (@ res status) ":"
                                (@ res body code)))
                            ((@ socket send) "echo")
                            ((@ socket on) "message"
                             (lambda (msg)
                               ((@ console log) msg)
                               ((@ process exit))))))))))))
      "null:400:3
echo"))

(with-server (:allow-upgrades nil)
  (diag "should allow arbitrary data through query string")
  (is (with-waiting
        (on :connection *server*
            (lambda (socket)
              (done (list (query-parameter (socket-request socket) "transport")
                          (query-parameter (socket-request socket) "a")))))
        (node-exec (with-client-socket (socket (ws-localhost "/" '(("a" . "b"))))
                     (set-timeout (lambda () ((@ process exit))) 500))))
      (list "polling" "b")))

(with-server (:allow-upgrades nil)
  (diag "should allow data through query in uri")
  (is (with-waiting
        (on :connection *server*
            (lambda (socket)
              (done (list (query-parameter (socket-request socket) "EIO")
                          (query-parameter (socket-request socket) "a")
                          (query-parameter (socket-request socket) "c")))))
        (node-exec (with-client-socket (socket (ws-localhost "/" '(("a" . "b") ("c" . "d"))))
                     (set-timeout (lambda () ((@ process exit))) 500))))
      (list "3" "b" "d")))

(with-server ()
  (diag "should disallow bad requests")
  (multiple-value-bind (body status headers)
      (http-get (localhost "/engine.io/default/"
                           '(("transport" . "websocket")))
                :headers '(("Origin" . "http://engine.io")))
    (is status 400)
    (is (jsown:val (jsown:parse body) "code") 3)
    (is (jsown:val (jsown:parse body) "message") "Bad request")
    (is (cdr (assoc :access-control-allow-credentials headers)) "true")
    (is (cdr (assoc :access-control-allow-origin headers)) "http://engine.io")))

(finalize)
