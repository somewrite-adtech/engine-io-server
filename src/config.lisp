(in-package :cl-user)
(defpackage engine-io-server.config
  (:use :cl
        :annot.class))
(in-package :engine-io-server.config)

(syntax:use-syntax :annot)

@export
@export-constructors
@export-accessors
(defstruct (config (:conc-name ""))
  (max-http-buffer-size 10e7 :type number)
  (available-transports '("websocket" "polling") :type list)
  (ping-timeout 60000 :type integer)
  (ping-interval 25000 :type integer)
  (allow-upgrades t :type boolean)
  (upgrade-timeout 10000 :type integer))
