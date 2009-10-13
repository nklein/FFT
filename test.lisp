(require :asdf)

#+sbcl
(require :sb-sprof)


(pushnew #P"/usr/local/asdf-install/site-systems/" asdf:*central-registry*)
(pushnew #P"." asdf:*central-registry*)

(defpackage #:fft-test
  (:use "COMMON-LISP"))

(in-package :fft-test)

(defmacro quietly-load (package)
  #+(or :abcl) `(asdf:operate 'asdf:load-op ,package)
  #-(or :abcl) `(asdf:operate 'asdf:load-op ,package :verbose nil))

(quietly-load 'fft)

#-(or :abcl (and :clozure :x8632-target) :cmu :ecl)
(quietly-load 'bordeaux-threads)

#+thread-support (quietly-load 'pcall)
#+thread-support (quietly-load 'pfft)

(defun make-random-buffer (dims &optional (random-state *random-state*))
  (let ((ret (make-array dims
			 :element-type '(complex double-float)
			 :initial-element (complex 0.0d0 0.0d0))))
    (flet ((number (random-state)
	     (- (random 2.0d0 random-state) 1.0d0)))
      (dotimes (ii (array-total-size ret) ret)
	(setf (row-major-aref ret ii) (complex (number random-state)
					       (number random-state)))))))
  
(defvar *dims* (list 512 512))
(defparameter *buf* (make-random-buffer *dims*))
(defparameter *dst* (make-array *dims*
				:element-type '(complex double-float)
				:initial-element (complex 0.0d0 0.0d0)))

;; run once to get the coefficients arrays initialized, then time it
(fft:fft *buf* *dst*)
(time (fft:fft *buf* *dst*))

;; if we've got threading, then time the parallel version, too.
#+thread-support
(time (pfft:pfft *buf* *dst*))

#|
;; run once to get the coefficients arrays initialized, then time it
(asdf:operate 'asdf:load-op 'bordeaux-fft :verbose nil)
(bordeaux-fft:fft! *buf* *dst*)
(time (bordeaux-fft:fft! *buf* *dst*))
|#

;; maybe we want to do profiling, too... I dunno...
#+(and :sbcl :not)
(sb-sprof:with-profiling (:max-samples 8000
			  :report :flat
			  :loop t)
  (fft:fft *buf* *dst*))
