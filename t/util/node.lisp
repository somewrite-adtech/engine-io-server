(in-package :cl-user)
(defpackage t-engine-io-server.util.node
  (:use :cl
        :parenscript
        :split-sequence)
  (:import-from :t-engine-io-server.util.url
                :ws-localhost)
  (:import-from :external-program
                :run
                :start)
  (:import-from :jsown
                :parse)
  (:export :*node-path*
           :*timeout-seconds*
           :node-exec
           :node-start
           :with-client-socket
           :close-client-socket))
(in-package :t-engine-io-server.util.node)

(defparameter *node-path* "/usr/local/bin/node")
(defparameter *timeout-seconds* 10)

(defun node-exec (js)
  (let* ((output (make-string-output-stream))
         (code (nth-value 1 (external-program:run
                             *node-path* `("-e" ,js)
                             :output output
                             :error *error-output*))))
    (unless (zerop code)
      (warn "Node.js process exited with ~D" code))
    (setq output
          (get-output-stream-string output))
    (if (zerop (length output))
        output
        (subseq output 0 (1- (length output))))))

(defun node-start (js &key output (error *error-output*))
  (external-program:start
   *node-path*
   `("-e"
     ,(format nil "~A;setTimeout(function(){},~D*1000);"
              js
              *timeout-seconds*))
   :output output
   :error error))

(defmacro with-client-socket ((name &optional url &rest args) &body body)
  `(ps (let ((,name
               (new ((@ (require "engine.io-client") -socket)
                     (lisp ,(or url '(ws-localhost)))
                     ,@(cond
                         ((or (equal args '(()))
                              (equal args '('())))
                          `((create)))
                         (args `((create ,@args)))
                         (T '()))))))
         (setf on (lambda (event socket fn)
                    ((@ socket on) event fn)))
         (setf once (lambda (event socket fn)
                      ((@ socket once) event fn)))
         (setf exit (lambda () ((@ process exit))))
         (setf abort (lambda () ((@ process abort))))
         ,@body
         (set-timeout (lambda () ((@ process abort))) (* ,*timeout-seconds* 1000)))))

(defmacro close-client-socket (socket)
  `((@ ,socket close)))

(import-macros-from-lisp 'close-client-socket)
