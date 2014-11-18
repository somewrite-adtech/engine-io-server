(in-package :cl-user)
(defpackage t-engine-io-server.util
  (:use :cl)
  (:import-from :engine-io-server
                :server
                :start-server
                :stop-server)
  (:import-from :drakma
                :http-request)
  (:import-from :usocket
                :socket-listen
                :socket-close
                :address-in-use-error)
  (:import-from :bordeaux-threads
                :with-timeout)
  (:import-from :ppcre
                :scan-to-strings)
  (:export :*server*
           :with-server
           :match
           :with-waiting
           :done))
(in-package :t-engine-io-server.util)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (cl-reexport:reexport-from :t-engine-io-server.util.node)
  (cl-reexport:reexport-from :t-engine-io-server.util.url)
  (cl-reexport:reexport-from :t-engine-io-server.util.http))

(defparameter *server* nil)

(defun port-available-p (port)
  (let (socket)
    (unwind-protect
         (handler-case (setq socket (usocket:socket-listen "127.0.0.1" port :reuse-address t))
           (usocket:address-in-use-error () nil))
      (when socket
        (usocket:socket-close socket)
        T))))

(defun random-port ()
  "Return a port number not in use from 50000 to 60000."
  (loop for port from (+ 50000 (random 1000)) upto 60000
        if (port-available-p port)
          return port))

(defmacro with-server ((&rest initargs) &body body)
  (let ((handler (gensym "HANDLER")))
    `(tagbody beginning
        (let* ((*test-port* (random-port))
               (*server* (make-instance 'server ,@initargs))
               (,handler (let ((*standard-output* (make-broadcast-stream)))
                           (start-server *server* :port *test-port*))))
          (sleep 1)
          (unwind-protect (handler-bind ((usocket:connection-refused-error
                                           (lambda (e)
                                             (declare (ignore e))
                                             (sleep 1)
                                             (go beginning))))
                            ;; Ensure the server is started successfully
                            (drakma:http-request (localhost))
                            ,@body)
            (stop-server ,handler)
            (sleep 0.5))))))

(defun match (target-string regex)
  (nth-value 1
             (ppcre:scan-to-strings regex target-string)))

(defparameter *timeout-seconds* 30)

(defmacro with-waiting (&body body)
  (let ((result (gensym "RESULT"))
        (finishedp (gensym "FINISHEDP")))
    `(let ((,result '#:unbound) (,finishedp nil))
       (flet ((done (&optional res)
                (setq ,finishedp t)
                (setq ,result res)))
         ,@body)
       (handler-case
           (bt:with-timeout (,*timeout-seconds*)
             (loop until ,finishedp do
               (sleep 0.1)))
         (bt:timeout () (warn "Timeout")))
       ,result)))
