(in-package :cl-user)
(defpackage engine-io-server.request
  (:use :cl
        :annot.class
        :split-sequence)
  (:import-from :engine-io-server.error
                :too-large-body)
  (:import-from :fast-io
                :with-fast-output
                :fast-write-sequence)
  (:import-from :babel
                :octets-to-string)
  (:import-from :alexandria
                :plist-hash-table))
(in-package :engine-io-server.request)

(syntax:use-syntax :annot)

@export
@export-accessors
(defstruct (request (:constructor %make-request)
                    (:conc-name ""))
  env
  raw-env
  request-method
  request-uri
  query-string
  request-socket
  %query-parameters)

@export
(defun make-request (env)
  (let* ((env-hash (plist-hash-table env :test 'eq))
         (uri (gethash :request-uri env-hash))
         (?-pos (position #\? uri :test #'char=))
         (query (if ?-pos
                    (subseq uri (1+ ?-pos))
                    nil)))
    (%make-request :env env-hash
                   :raw-env env
                   :request-method (gethash :request-method env-hash)
                   :request-uri (gethash :request-uri env-hash)
                   :query-string query
                   :%query-parameters (parse-query-string query))))

@export
(defun query-parameter (req name)
  (gethash name (%query-parameters req)))

@export
(defun read-body-as-binary (req &key max-size)
  (with-fast-output (chunks :vector)
    (let* ((env (env req))
           (body (gethash :raw-body env))
           (buffer-size (or (gethash :content-length env)
                            1024))
           (read-length 0))
      (loop
        (let* ((buffer (make-array buffer-size :element-type '(unsigned-byte 8)))
               (read-pos (read-sequence buffer body)))
          (fast-write-sequence (adjust-array buffer read-pos) chunks)
          (unless (= read-pos buffer-size)
            (return))
          (incf read-length read-pos)
          (unless (<= read-length max-size)
            (error 'too-large-body)))))))

@export
(defun read-body-as-text (req &key max-size)
  (octets-to-string (read-body-as-binary req :max-size max-size)))

@export
(defun read-body (req &key max-size)
  (if (string= (gethash :content-type (env req))
               "application/octet-stream")
      (read-body-as-binary req :max-size max-size)
      (read-body-as-text req :max-size max-size)))

(defun parse-query-string (query)
  (let ((hash (make-hash-table :test 'equal)))
    (when query
      (dolist (kv (split-sequence #\& query))
        (multiple-value-bind (key pos)
            (split-sequence #\= kv :count 1)
          (setf (gethash (car key) hash) (subseq kv pos)))))
    hash))
