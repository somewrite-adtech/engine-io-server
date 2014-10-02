(in-package :cl-user)
(defpackage engine-io-server.timer
  (:use :cl)
  (:import-from :cl-async
                :interval
                :remove-interval
                :event-freed)
  (:import-from :alexandria
                :when-let)
  (:export :timers
           :make-timers
           :start-timer
           :stop-timer))
(in-package :engine-io-server.timer)

(defstruct timers
  check-interval
  ping-timeout
  upgrade-timeout)

(defun start-timer (timers name fn &key time)
  (when (keywordp name)
    (setf name (intern (symbol-name name) #.*package*)))
  (when-let (interval (slot-value timers name))
    (handler-case (as:remove-interval interval)
      (as:event-freed ())))
  (setf (slot-value timers name)
        (as:interval fn :time time)))

(defun stop-timer (timers name)
  (when (keywordp name)
    (setf name (intern (symbol-name name) #.*package*)))
  (when-let (interval (slot-value timers name))
    (handler-case (as:remove-interval interval)
      (as:event-freed ()))
    (setf (slot-value timers name) nil)
    T))
