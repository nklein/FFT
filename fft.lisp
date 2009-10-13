
(in-package #:fft)

(define-modify-macro multf (&optional (factor (complex 1.0d0 0.0d0))) *)

(defun shift-samples (row)
  (declare (optimize (speed 3))
	   (type (simple-array (complex double-float) *) row))
  (let* ((len (length row))
	 (mid (ash len -1)))
    (declare (type fixnum len mid))
    (dotimes (ii mid row)
      (declare (type fixnum ii))
      (rotatef (aref row ii) (aref row (the fixnum (+ ii mid)))))))

(defvar *fft-info* nil)
(defvar *ifft-info* nil)

(defun calculate-coefficients (coeffs inverse)
  (declare (type (simple-array (complex double-float) *) coeffs))
  (let* ((direction (if inverse 1.0d0 -1.0d0))
	 (base-angle (/ (* direction pi (complex 0.0d0 1.0d0))
			(length coeffs))))
    (declare (type double-float direction)
	     (type (complex double-float) base-angle))
    (loop :for ii :from 0 :below (length coeffs)
       :do (setf (aref coeffs ii) (exp (* base-angle ii))))))

(defun get-fft-buffers (length inverse)
  (let ((info (if inverse *ifft-info* *fft-info*)))
    (unless (and info (= (first info) length))
      (let ((info (list length
			(make-array length
				    :element-type '(complex double-float))
			(make-array length
				    :element-type '(complex double-float))
			(make-array length
				    :element-type '(complex double-float))
			(make-array (ash length -1)
				    :element-type '(complex double-float)))))
	(calculate-coefficients (fifth info) inverse)
	(if inverse
	    (setf *ifft-info* info)
	    (setf *fft-info* info)))))
  (let ((info (if inverse *ifft-info* *fft-info*)))
    (values-list (rest info))))

(declaim (ftype (function (virtual-row (simple-array (complex double-float) *))
			  (simple-array (complex double-float) *))
		row-to-array))
(defun row-to-array (row array)
  (declare (type virtual-row row)
	   (type (simple-array (complex double-float) *) array)
	   (optimize (speed 3)))
  (assert (= (row-length row) (length array)))
  (dotimes (ii (row-length row) array)
    (declare (type length-type ii))
    (let ((vv (row-ref row ii)))
      (declare (type (complex double-float) vv))
      (setf (aref array ii) vv))))

(declaim (ftype (function ((simple-array (complex double-float) *)
			   virtual-row
			   &optional double-float) virtual-row) array-to-row))
(defun array-to-row (array row &optional (scale 1.0d0))
  (declare (type (simple-array (complex double-float) *) array)
	   (type virtual-row row)
	   (type double-float scale)
	   (optimize (speed 3)))
  (assert (= (row-length row) (length array)))
  (dotimes (ii (row-length row) row)
    (let* ((vv (aref array ii))
	   (vs (* vv scale)))
      (declare (type (complex double-float) vv vs))
      (set-row-ref row ii vs))))

(defun perform-fft (row &optional inverse)
  (declare (optimize (speed 3))
	   (type virtual-row row)
	   (type t inverse))
  (let ((length (row-length row)))
    (declare (type length-type length))
    (assert (zerop (logand length (1- length)))
	    (length)
	    "Row length must be power of two, but is ~S" length)
    (multiple-value-bind (src tmp dst coeffs)
	(get-fft-buffers length inverse)
      (declare (type (simple-array (complex double-float) *)
		     src tmp dst coeffs))
      (labels ((calc (x-buf x-index length skip
			    out-buf out-index d-buf d-index)
		 (declare (type (simple-array (complex double-float) *)
				x-buf out-buf d-buf)
			  (type length-type x-index length skip 
				            out-index d-index)
			  (optimize (speed 3) (safety 0)))
		 (cond
		   ((= length 1) (setf (aref out-buf out-index)
				       (aref x-buf x-index)))
		   (t (let* ((half    (ash length -1))
			     (2-skips (ash skip 1))
			     (new-d   (the length-type (+ d-index half)))
			     (new-x   (the length-type (+ x-index skip))))
			(declare (type length-type half 2-skips new-d new-x))
			(calc x-buf x-index half 2-skips
			      d-buf new-d   out-buf out-index)
			(calc x-buf new-x   half 2-skips
			      d-buf d-index out-buf out-index)
			(loop :for kk :of-type length-type :from 0 :below half
			   :for c-index :of-type length-type :from 0 :by skip
			   :for di :of-type fixnum :from d-index
			   :do (multf (aref d-buf di)
				      (aref coeffs c-index)))
			(loop :for kk :of-type fixnum :from 0 :below half
			   :do (let ((ee (aref d-buf (+ kk d-index half)))
				     (dd (aref d-buf (+ kk d-index))))
				 (declare (type (complex double-float) ee dd))
				 (setf (aref out-buf (+ kk out-index))
				       (+ ee dd))
				 (setf (aref out-buf (+ kk out-index half))
				       (- ee dd)))))))
		 (values)))
        (row-to-array row src)
	(when inverse
	  (shift-samples src))
	(calc src 0 length 1 dst 0 tmp 0)
	(unless inverse
	  (shift-samples dst))
	(array-to-row dst row
		      (the double-float
			(/ 1.0d0 (coerce (sqrt length) 'double-float)))))))
  row)

(defun make-dst-buf (src dst)
  (declare (optimize (speed 3))
	   (type (simple-array (complex double-float) *) src)
	   (type (or (simple-array (complex double-float) *) null) dst))
  (labels ((copy-array (src dst)
	     (declare (type (simple-array (complex double-float) *) src dst))
	     (let ((size (array-total-size src)))
	       (dotimes (ii size dst)
		 (let ((vv (row-major-aref src ii)))
		   (declare (type (complex double-float) vv))
		   (setf (row-major-aref dst ii) vv)))))
	   (clone-array (src)
	     (let ((dst (make-array (array-dimensions src)
				    :element-type '(complex double-float))))
		 (copy-array src dst))))
    (cond
      ((null dst) (clone-array src))
      ((eq src dst) dst)
      ((equal (array-dimensions src)
	      (array-dimensions dst)) (copy-array src dst))
      (t (error "Incompatible source and destination")))))

(defun fft (src &optional dst)
  (let ((dst (make-dst-buf src dst)))
    (loop :for row = (virtual-row dst) :then (next-row row)
       :while row
       :do (perform-fft row nil))
    dst))

(defun ifft (src &optional dst)
  (let ((dst (make-dst-buf src dst)))
    (loop :for row = (virtual-row dst) :then (next-row row)
       :while row
       :do (perform-fft row t))
    dst))
