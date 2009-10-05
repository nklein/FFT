
(in-package #:fft)

(defun rearrange-samples (row)
  (labels ((next-target (target mask)
	     (let ((mask (ash mask -1)))
	       (cond
		 ((zerop (logand target mask)) (logior target mask))
		 (t (next-target (logandc2 target mask) mask))))))
    (loop :with len = (row-length row)
       :for target = 0 :then (next-target target len)
       :for pos :from 0 :below len
       :do (when (< target pos)
	     (rotatef (row-ref row target) (row-ref row pos)))))
  row)

(defun shift-samples (row)
  (loop :with len = (row-length row)
     :with mid = (ash len -1)
     :for ii :from 0 :below mid
     :for jj :from mid :below len
     :do (rotatef (row-ref row ii) (row-ref row jj)))
  row)

(defun scale-samples (row)
  (loop :with len = (row-length row)
     :with scale = (/ (sqrt len))
     :for ii :from 0 :below len
     :do (setf (row-ref row ii) (* scale (row-ref row ii))))
  row)

(deftype small-double-float ()
  '(double-float #.(- pi) #.pi))

(defun perform-fft (row &optional inverse)
  (assert (zerop (logand (row-length row)
			 (1- (row-length row)))))
  (when inverse
    (shift-samples row))
  (rearrange-samples row)
  (loop :with angular-scale = (if inverse pi (- pi))
     :with len = (row-length row)
     :for step = 1 :then (* step 2)
     :while (< step len)
     :do (loop :with delta = (/ angular-scale step)
	    :with sine = (sin (the small-double-float (/ delta 2.0)))
	    :with multiplier = (complex (* -2.0 sine sine)
					(sin (the small-double-float delta)))
	    :for factor = (complex 1.0 0.0) :then (+ factor
						     (* factor multiplier))
	    :for group :from 0 :below step
	    :do (loop :for pair :from group :below len :by (* step 2)
		   :do (let ((match (+ pair step)))
			 (let ((product (* factor (row-ref row match))))
			   (setf (row-ref row match)
				 (- (row-ref row pair) product))
			   (incf (row-ref row pair) product))))))
  (scale-samples row)
  (unless inverse
    (shift-samples row))
  row)

(defun make-dst-buf (src dst)
  (flet ((clone-array (src)
	   (let ((dims (array-dimensions src)))
	     (adjust-array (make-array dims
				       :displaced-to src)
			   dims)))
	 (copy-array (src dst)
	   (let ((size (array-total-size src)))
	     (let ((src-linear (make-array size :displaced-to src))
		   (dst-linear (make-array size :displaced-to dst)))
	       (replace dst-linear src-linear)))))
    (cond
      ((eq src dst) dst)
      ((null dst) (clone-array src))
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
