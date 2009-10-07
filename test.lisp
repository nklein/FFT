(require :asdf)

#+sbcl
(require :sb-sprof)


(pushnew #P"/usr/local/asdf-install/site-systems/" asdf:*central-registry*)
(pushnew #P"/Users/pat/.sbcl/systems/" asdf:*central-registry*)

(defpackage #:fft-test
  (:use "COMMON-LISP"))

(in-package :fft-test)

(asdf:operate 'asdf:load-op 'fft :verbose nil)
(asdf:operate 'asdf:load-op 'bordeaux-threads :verbose nil)
(asdf:operate 'asdf:load-op 'pcall :verbose nil)
(asdf:operate 'asdf:load-op 'pfft :verbose nil)

(defun make-random-buffer (dims &optional (random-state *random-state*))
  (let ((ret (make-array dims
			 :element-type '(complex double-float)
			 :initial-element (complex 0.0d0 0.0d0))))
    (flet ((number (random-state)
	     (- (random 2.0d0 random-state) 1.0d0)))
      (dotimes (ii (array-total-size ret) ret)
	(setf (row-major-aref ret ii) (complex (number random-state)
					       (number random-state)))))))
  
(defvar *dims* '(512 512))
(defparameter *buf* (make-random-buffer *dims*))
(defparameter *dst* (make-array *dims*
				:element-type '(complex double-float)
				:initial-element (complex 0.0d0 0.0d0)))

(time (fft:fft *buf* *dst*))
#-sbcl
(time (pfft:pfft *buf* *dst*))

#+(and :sbcl :not)
(sb-sprof:with-profiling (:max-samples 8192
			  :report :flat
			  :loop t)
  (fft:fft *buf* *dst*))
