;;;; test.lisp

(in-package #:jaganet-client)
(setq lisp-unit:*print-failures* t
      lisp-unit:*print-errors* t)

(lisp-unit:define-test test-add-time
  (defparameter *time-remaining* 0)
  (defparameter *status* 'bla)
  (add-time 7)
  (lisp-unit:assert-equal 7 *time-remaining*)
  (lisp-unit:assert-eql 'limited-time *status*)

  (add-time 8)
  (lisp-unit:assert-equal 15 *time-remaining*)
  (lisp-unit:assert-eql 'limited-time *status*)

  (lisp-unit:assert-error 'type-error (add-time "two"))
  (lisp-unit:assert-error 'type-error (add-time '(2))))

(lisp-unit:define-test test-process-message
  (defparameter *time-remaining* 0)
  (defparameter *status* 'bla)

  (process-message '(:add-time 5))
  (lisp-unit:assert-equal '5 *time-remaining*)
  (lisp-unit:assert-eql 'limited-time *status*)

  (process-message '(:stop))
  (lisp-unit:assert-eql 'stopped *status*)
  (defparameter *status* 'new))


(lisp-unit:define-test test-set-config
  (set-config '(:server-address "127.0.0.1" :server-port 4321))
  (lisp-unit:assert-equal "127.0.0.1" *server-address*)
  (lisp-unit:assert-equal 4321 *server-port*))

(lisp-unit:define-test test-timer
  (setf *status* 'limited-time)
  (start-timer)
  (lisp-unit:assert-equal *start-time* (get-universal-time))

  (pause-timer)
  (lisp-unit:assert-equal *last-pause-time* (get-universal-time))
  (lisp-unit:assert-eql *status-before-pause* 'limited-time)
  (lisp-unit:assert-eql *status* 'paused)

  (sleep 3)

  (unpause-timer)
  (lisp-unit:assert-equal *seconds-paused* (- (get-universal-time) *last-pause-time*))
  (lisp-unit:assert-equal (get-seconds-used) (- (- (get-universal-time) *start-time*)
                                                *seconds-paused*))
  (lisp-unit:assert-eql *status* 'limited-time))

(lisp-unit:define-test test-stop
  (defparameter *status* 'bla)
  (stop)
  (lisp-unit:assert-equal 'stopped *status*)
  (defparameter *status* 'new))

