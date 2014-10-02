#|
  This file is a part of engine-io-server project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage engine-io-server-asd
  (:use :cl :asdf))
(in-package :engine-io-server-asd)

(defsystem engine-io-server
  :version "0.0.1"
  :author "Eitaro Fukamachi"
  :depends-on (:engine-io-parser
               :event-emitter
               :websocket-driver
               :cl-async
               :clack
               :jsown
               :unicly
               :babel
               :alexandria
               :split-sequence
               :cl-syntax-annot
               :cl-reexport
               :log4cl)
  :components ((:module "src"
                :components
                ((:file "engine-io-server" :depends-on ("server"))
                 (:file "server" :depends-on ("socket" "transport" "error" "request" "config"))
                 (:file "socket" :depends-on ("transport" "config" "timer" "request"))
                 (:file "config")
                 (:file "request" :depends-on ("error"))
                 (:file "transport" :depends-on ("error" "request"))
                 (:module "transport-components"
                  :pathname "transport"
                  :depends-on ("transport" "request")
                  :components
                  ((:file "polling")
                   (:file "polling-xhr" :depends-on ("polling"))
                   (:file "websocket")))
                 (:file "timer")
                 (:file "error"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.md"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op t-engine-io-server))))
