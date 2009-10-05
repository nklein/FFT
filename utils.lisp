
(in-package #:fft)

(defstruct virtual-row
  buffer
  dimension
  base)

(defun virtual-row (buffer)
  (make-virtual-row :buffer buffer
		    :dimension 0
		    :base (mapcar (constantly 0)
				  (array-dimensions buffer))))

(defun row-length (row)
  (with-slots (buffer dimension) row
    (array-dimension buffer dimension)))

(defun row-ref (row index)
  (with-slots (buffer dimension base) row
    (setf (nth dimension base) index)
    (apply #'aref buffer base)))

(defun set-row-ref (row index value)
  (with-slots (buffer dimension base) row
    (setf (nth dimension base) index)
    (eval `(setf (aref ,buffer ,@base) ,value))))

(defsetf row-ref set-row-ref)

(defun next-row (row)
  (labels ((inc-within-dimension (row index)
	     (with-slots (buffer dimension base) row
	       (cond
		 ((minusp index) nil)
		 ((= dimension index) (inc-within-dimension row (1- index)))
		 ((>= (incf (nth index base))
		      (array-dimension buffer index))
		                      (setf (nth index base) 0)
		                      (inc-within-dimension row (1- index)))
		 (t row))))
	   (inc-counter (row)
	     (with-slots (buffer dimension base) row
	       (let ((cntr (inc-within-dimension
			       row
			       (1- (length (array-dimensions buffer))))))
		 (cond
		   (cntr cntr)
		   ((>= (incf dimension)
			(length (array-dimensions buffer))) nil)
		   (t (loop :for bb :on base
			 :do (setf (car bb) 0))
		      row))))))
    (when (inc-counter row)
      row)))
