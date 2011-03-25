(require :asdf)

#+sbcl (require :sb-aclrepl)

(pushnew #P"/usr/local/asdf-install/site-systems/" asdf:*central-registry*)

(defmacro load-package (package &optional verbosity)
  (declare (ignorable verbosity))
  #+(or :abcl) `(asdf:operate 'asdf:load-op ,package)
  #-(or :abcl) `(asdf:operate 'asdf:load-op ,package :verbose ,verbosity))

(load-package 'nst)

#-(or :abcl :clisp (and :clozure :x8632-target) :cmu :ecl)
(load-package 'bordeaux-threads)

#+thread-support (load-package 'pcall)

(pushnew #P"." asdf:*central-registry*)

(load-package 'fft t)
#+thread-support (load-package 'pfft t)

(defpackage #:regression-test
  (:use :cl))

(in-package :regression-test)

(defvar +epsilon+ 0.000001)

(defun buffers-nearly-identical (aa bb)
  (flet ((rms-diff (aa bb)
	   (let ((rms 0.0d0))
	     (dotimes (ii (array-total-size aa) rms)
	       (let ((delta (- (row-major-aref aa ii)
			       (row-major-aref bb ii))))
		 (incf rms (abs delta)))))))
    (and (equal (array-dimensions aa) (array-dimensions bb))
	 (< (rms-diff aa bb) +epsilon+))))

(nst:def-fixtures simple-buffers ()
  (one-sample #(#C(1 2)))
  (one-by-one #2A((#C(1 2))))
  (four-ascending #( #C(1 2) #C(3 4) #C(5 6) #C(7 8) ))
  (four-asc-trans #( #C(-2 -2) #C(0 -4) #C(8 10) #C(-4 0) )))

(nst:def-fixtures two-dimensional-buffers ()
  (four-by-four #2A(( 2 0           0 0)
		    ( 5 #C(1 1)    -1 #C(1 -1))
		    (15 #C(4 6)    -5 #C(4 -6))
		    (50 #C(13 28) -22 #C(13 -28))))
  (four-by-four-trans
                #2A((0 -2.5 -10 -25.5)
		    (0 #C(-1.5 -3.0) #C(-4 -12) #C(-7.5 -30))
		    (2 7.5 20.0 42.5)
		    (0 #C(-1.5 3.0) #C(-4 12) #C(-7.5 30)))))

(nst:def-test-group simple-tests (simple-buffers)
  (nst:def-test one-sample-fft :true
    (buffers-nearly-identical (fft:fft one-sample) one-sample))
  (nst:def-test one-sample-ifft :true
    (buffers-nearly-identical (fft:ifft one-sample) one-sample))
  (nst:def-test one-by-one-fft :true
    (buffers-nearly-identical (fft:fft one-by-one) one-by-one))
  (nst:def-test one-by-one-ifft :true
    (buffers-nearly-identical (fft:ifft one-by-one) one-by-one))
  (nst:def-test four-asc-fft :true
    (buffers-nearly-identical (fft:fft four-ascending) four-asc-trans))
  (nst:def-test four-asc-ifft :true
    (buffers-nearly-identical (fft:ifft four-asc-trans) four-ascending)))

(nst:def-test-group two-dimensional-tests (two-dimensional-buffers)
  (nst:def-test forward-four-by-four :true
    (buffers-nearly-identical (fft:fft four-by-four) four-by-four-trans))
  (nst:def-test inverse-four-by-four :true
    (buffers-nearly-identical (fft:ifft four-by-four-trans) four-by-four)))

#+thread-support
(nst:def-test-group two-dimensional-parallel-tests (two-dimensional-buffers)
  (nst:def-test parallel-forward-four-by-four :true
    (buffers-nearly-identical (pfft:pfft four-by-four) four-by-four-trans))
  (nst:def-test parallel-inverse-four-by-four :true
    (buffers-nearly-identical (pfft:pifft four-by-four-trans) four-by-four)))

(nst:nst-cmd :set :debug-on-error :debug)
(nst:nst-cmd :run-package :regression-test)
