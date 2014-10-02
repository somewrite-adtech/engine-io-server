(in-package :cl-user)
(defpackage t-engine-io-server.util.http
  (:use :cl)
  (:import-from :drakma
                :http-request)
  (:import-from :usocket
                :connection-refused-error)
  (:import-from :babel
                :octets-to-string)
  (:export :http-get))
(in-package :t-engine-io-server.util.http)

(defun http-get (url &key headers)
  (tagbody requesting
     (multiple-value-bind (body status res-headers)
       (restart-case
           (http-request url
                         :additional-headers headers
                         :connection-timeout 5)
         (retry-http-request ()
           :report "Retry the HTTP request"
           (go requesting)))
       (return-from http-get
         (values
          (if (string= (cdr (assoc :content-type res-headers))
                       "application/json")
              (babel:octets-to-string body :encoding :utf-8)
              body)
          status
          res-headers)))))
