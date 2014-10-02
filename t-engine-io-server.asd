#|
  This file is a part of engine-io-server project.
  Copyright (c) 2014 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage t-engine-io-server-asd
  (:use :cl :asdf))
(in-package :t-engine-io-server-asd)

(defsystem t-engine-io-server
  :author "Eitaro Fukamachi"
  :depends-on (:engine-io-server
               :prove
               :drakma
               :babel
               :cl-ppcre
               :jsown
               :parenscript
               :external-program
               :usocket
               :split-sequence
               :cl-reexport)
  :components ((:module "t"
                :components
                ((:module "server"
                  :depends-on ("util")
                  :serial t
                  :components
                  ((:test-file "verification")
                   (:test-file "handshake")
                   (:test-file "close")
                   (:test-file "message")
                   (:test-file "send")
                   (:test-file "packet")
                   (:test-file "packet-create")
                   (:test-file "upgrade")))
                 (:file "util" :depends-on ("util-packages"))
                 (:module "util-packages"
                  :pathname "util"
                  :components
                  ((:file "node" :depends-on ("url"))
                   (:file "url")
                   (:file "http"))))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
