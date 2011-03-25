
(defpackage #:pfft
  (:use :cl)
  (:export #:pfft #:pifft)
  (:import-from :fft :perform-fft :virtual-row :next-row :make-dst-buf))

(in-package #:pfft)

(defun perform-parallel-rows (row inverse &optional task-list)
  (when row
    (let* ((task (pcall:pexec (perform-fft row inverse)))
	   (task-list (cons task task-list)))
      (multiple-value-bind (next-row advanced) (next-row row)
	(cond
	  ((or advanced (null next-row))
	      (mapc #'pcall:join task-list)
	      (perform-parallel-rows next-row inverse))
	  (t  (perform-parallel-rows next-row inverse task-list)))))))

(defun pfft (src &optional dst)
  #-thread-support (fft:fft src dst)
  #+thread-support (let ((dst (make-dst-buf src dst)))
		     (perform-parallel-rows (virtual-row dst) nil)
		     dst))

(defun pifft (src &optional dst)
  #-thread-support (fft:ifft src dst)
  #+thread-support (let ((dst (make-dst-buf src dst)))
		     (perform-parallel-rows (virtual-row dst) t)
		     dst))
