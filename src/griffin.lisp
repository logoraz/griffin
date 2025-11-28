(defpackage #:griffin
  (:nicknames #:grif)
  (:use #:cl
        #:ui/app)
  (:local-nicknames (#:it #:iterate))
  ;; Play
  (:export #:simple-test
           #:simple-test2)
  ;; External API
  (:export #:ui)
  (:documentation "Griffin"))

(in-package #:griffin)


;;; =============================================================================
;;; Play 
;;; =============================================================================
(defun simple-test (&optional (n 11))
  "Simple function for testing."
  (loop :for i :from 0 :below n
        :collect (list (format nil "list ~A" i)
                       (/ i n))))

(defun simple-test2 (&optional (n 11))
  "Simple function for testing."
  (it:iter (it:for i from 0 below n)
           (it:collect (list (format nil "list ~A" i)
                             (/ i n)))))

;;; =============================================================================
;;; Entry Point
;;; =============================================================================
(defun ui ()
  "Main entry point for the executable."
  (start-app))


