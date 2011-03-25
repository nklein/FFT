(require :asdf)

#+sbcl
(require :sb-sprof)


(pushnew #P"/usr/local/asdf-install/site-systems/" asdf:*central-registry*)
(pushnew #P"./" asdf:*central-registry*)

(defpackage #:fft-test
  (:use :cl))

(in-package :fft-test)

(defmacro load-package (package &optional verbosity)
  (declare (ignorable verbosity))
  #+(or :abcl) `(asdf:operate 'asdf:load-op ,package)
  #-(or :abcl) `(asdf:operate 'asdf:load-op ,package :verbose ,verbosity))

(load-package 'fft t)
(load-package 'bordeaux-fft)

#-(or :abcl (and :clozure :x8632-target) :cmu :ecl)
(load-package 'bordeaux-threads)

#+thread-support (load-package 'pcall)
#+thread-support (load-package 'pfft)

(defun make-random-buffer (dims &optional (random-state *random-state*))
  (let ((ret (make-array dims
			 :element-type '(complex double-float)
			 :initial-element (complex 0.0d0 0.0d0))))
    (flet ((number (random-state)
	     (- (random 2.0d0 random-state) 1.0d0)))
      (dotimes (ii (array-total-size ret) ret)
	(setf (row-major-aref ret ii) (complex (number random-state)
					       (number random-state)))))))

(dolist (dims '((1048576) (512 512) (1024 1024) (256 256 64)))
  (let ((buf (make-random-buffer dims))
	(dst (make-array dims :element-type '(complex double-float))))
    (format t "DIMS: ~S~%" dims)
    ;; run once to get the coefficients arrays initialized, then time it
    (fft:fft buf dst)
    (time (fft:fft buf dst))

    ;; if we've got threading, then time the parallel version, too.
    #+thread-support
    (time (pfft:pfft buf dst))

    ;; run once to get the coefficients arrays initialized, then time it
    (when (= (length dims) 1)
      (bordeaux-fft:fft! buf dst)
      (time (bordeaux-fft:fft! buf dst)))

    ;; maybe we want to do profiling, too... I dunno...
    #+(and :sbcl :not)
    (sb-sprof:with-profiling (:max-samples 8000
			      :report :flat
			      :loop t)
      (fft:fft buf dst))))
