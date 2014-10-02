(in-package :cl-user)
(defpackage t-engine-io-server.util.url
  (:use :cl)
  (:export :*test-port*
           :localhost
           :ws-localhost))
(in-package t-engine-io-server.util.url)

(defparameter *test-port* nil)

(defun make-localhost-url (&key (protocol "http") (path "/") query)
  (let ((query (and query
                    (format nil "~{~A~^&~}"
                            (loop for (key . val) in query
                                  collect (format nil "~A=~A" key val))))))
    (format nil "~(~A~)://localhost:~D~A~:[~;~:*?~A~]"
            protocol
            *test-port*
            path
            query)))

(defun localhost (&optional (path "/") query)
  (make-localhost-url :path path :query query))

(defun ws-localhost (&optional (path "/") query)
  (make-localhost-url :protocol "ws" :path path :query query))
