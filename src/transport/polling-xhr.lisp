(in-package :cl-user)
(defpackage engine-io-server.transport.polling-xhr
  (:use :cl)
  (:import-from :engine-io-server.transport
                :request
                :response
                :on-request
                :write-data
                :headers)
  (:import-from :engine-io-server.transport.polling
                :polling)
  (:import-from :engine-io-server.request
                :request-method
                :env))
(in-package :engine-io-server.transport.polling-xhr)

(syntax:use-syntax :annot)

@export
(defclass polling-xhr (polling) ())

(defmethod on-request ((polling polling-xhr) req res)
  (if (eq (request-method req)
          :options)
      (funcall res
               (list 200
                     (append (headers polling)
                             (list :access-control-allow-headers "Content-Type"))
                     '("")))
      (call-next-method)))

(defmethod write-data ((polling polling-xhr) data)
  (let ((content-type (if (stringp data)
                          "text/plain; charset=UTF-8"
                          "application/octet-stream")))


    ;; TODO: prevent XSS warnings on IE

    (funcall (response polling)
             (list 200
                   (append (headers polling)
                           (list :content-type content-type
                                 :content-length (length data)))
                   (if (stringp data)
                       (list data)
                       data)))))

(defmethod headers ((polling polling-xhr))
  (let* ((req (request polling))
         (origin (gethash :origin (env req))))
    (if origin
        (list :access-control-allow-credentials t
              :access-control-allow-origin origin)
        (list :access-control-allow-origin "*"))))
