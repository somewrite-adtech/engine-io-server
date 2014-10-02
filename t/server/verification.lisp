(in-package :cl-user)
(defpackage t-engine-io-server.server.verification
  (:use :cl
        :t-engine-io-server.util
        :engine-io-server
        :prove
        :jsown))
(in-package :t-engine-io-server.server.verification)

(plan 14)

(diag "- verification")

(with-server ()
  (diag "should disallow non-existent transports")
  (multiple-value-bind (body status headers)
      (http-get (localhost "/engine.io/default/"
                           '(("transport" . "tobi"))))
    (is status 400)
    (is (val (parse body) "code") 0)
    (is (val (parse body) "message") "Transport unknown")
    (is (cdr (assoc :access-control-allow-origin headers))
        "*")))

(with-server ()
  (diag "should disallow `constructor` as transports")
  (multiple-value-bind (body status headers)
      (http-get (localhost "/engine.io/default/"
                           '(("transport" . "contructor")))
                :headers '(("Origin" . "http://engine.io")))
    (is status 400)
    (is (val (parse body) "code") 0)
    (is (val (parse body) "message") "Transport unknown")
    (is (cdr (assoc :access-control-allow-credentials headers))
        "true")
    (is (cdr (assoc :access-control-allow-origin headers))
        "http://engine.io")))

(with-server ()
  (diag "should disallow non-existent sids")
  (multiple-value-bind (body status headers)
      (http-get (localhost "/engine.io/default"
                           '(("transport" . "polling")
                             ("sid" . "test")))
                :headers '(("Origin" . "http://engine.io")))
    (is status 400)
    (is (val (parse body) "code") 1)
    (is (val (parse body) "message") "Session ID unknown")
    (is (cdr (assoc :access-control-allow-credentials headers))
        "true")
    (is (cdr (assoc :access-control-allow-origin headers))
        "http://engine.io")))

(finalize)
