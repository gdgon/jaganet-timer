
(in-package #:jaganet-client)
(setq lisp-unit:*print-failures* t
      lisp-unit:*print-errors* t)

(lisp-unit:define-test test-add-time
  (defparameter *time-remaining* 0)
  (defparameter *status* 'bla)
  (add-time 7)
  (lisp-unit:assert-equal 7 *time-remaining*)
  (lisp-unit:assert-eql 'limited-session *status*)

  (add-time 8)
  (lisp-unit:assert-equal 15 *time-remaining*)
  (lisp-unit:assert-eql 'limited-session *status*)

  (lisp-unit:assert-error 'type-error (add-time "two"))
  (lisp-unit:assert-error 'type-error (add-time '(2))))

(lisp-unit:define-test test-stop
  (defparameter *status* 'bla)
  (stop)
  (lisp-unit:assert-equal 'stopped *status*))

(lisp-unit:define-test test-process-message
  (defparameter *time-remaining* 0)
  (defparameter *status* 'bla)

  (process-message '(:add-time 5))
  (lisp-unit:assert-equal '5 *time-remaining*)
  (lisp-unit:assert-eql 'limited-session *status*)

  (process-message '(:stop))
  (lisp-unit:assert-eql 'stopped *status*))
