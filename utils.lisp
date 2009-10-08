
(in-package #:fft)

(defstruct virtual-row
  length
  getter
  setter
  next)

(defun vr-aref (buffer pre post)
  (let ((make-closure
	   (compile nil `(lambda (buf)
			   (lambda (index)
			     (aref buf ,@pre index ,@post))))))
    (compile nil (funcall make-closure buffer))))

(defun vr-setf-aref (buffer pre post)
  (let ((make-closure
	   (compile nil `(lambda (buf)
			   (lambda (index value)
			     (setf (aref buf ,@pre index ,@post) value))))))
    (compile nil (funcall make-closure buffer))))

(defun vr-next-row (buffer pre post)
  (labels ((inc-pre (buffer pre &optional (index 0))
	     (cond
	       ((<= (length pre) index) nil)
	       ((< (incf (nth index pre))
		   (array-dimension buffer index)) pre)
	       (t (setf (nth index pre) 0)
		  (inc-pre buffer pre (1+ index)))))
	   (inc-post (buffer post offset &optional (index 0))
	     (cond
	       ((<= (length post) index) nil)
	       ((< (incf (nth index post))
	
	   (array-dimension buffer (+ index offset))) post)
	       (t (setf (nth index post) 0)
		  (inc-post buffer post offset (1+ index)))))
	   (inc (buffer pre post)
	     (let* ((new-pre  (inc-pre buffer pre))
		    (new-post (if new-pre
				  post
				  (inc-post buffer post (1+ (length pre))))))
	       (cond
		 (new-pre  (values new-pre post nil))
		 (new-post (values (mapcar (constantly 0) pre)
				   new-post
				   nil))
		 ((<= (length (array-dimensions buffer))
		      (1+ (length pre))) (values nil nil nil))
		 (t (values (cons 0 (mapcar (constantly 0) pre))
			    (mapcar (constantly 0) (cdr post))
			    t))))))
    (inc buffer (copy-seq pre) (copy-seq post))))

(defun virtual-row (buffer &optional (dimension 0) pre post)
  (declare (type (array (complex double-float)) buffer)
	   (type fixnum dimension)
	   (type list pre post))
  (let ((pre  (or pre
		  (mapcar (constantly 0)
			  (subseq (array-dimensions buffer) 0 dimension))))
	(post (or post
		  (mapcar (constantly 0)
			  (subseq (array-dimensions buffer) (1+ dimension))))))
    (make-virtual-row :length (array-dimension buffer dimension)
		      :getter (vr-aref buffer pre post)
		      :setter (vr-setf-aref buffer pre post)
		      :next   (compile nil
				       (lambda ()
					 (multiple-value-bind (new-pre
							       new-post
							       advance)
					     (vr-next-row buffer pre post)
					   (when (or new-pre new-post)
					     (values
					        (virtual-row buffer
							     (length new-pre)
							     new-pre
							     new-post)
						advance))))))))

(declaim (ftype (function (virtual-row) fixnum) row-length))
(defun row-length (row)
  (declare (type virtual-row row))
  (virtual-row-length row))

(declaim (ftype (function (virtual-row fixnum)
			  (complex double-float)) row-ref))
(defun row-ref (row index)
  (declare (type virtual-row row)
	   (type fixnum index))
  (nth-value 0 (funcall (virtual-row-getter row) index)))

(declaim (ftype (function (virtual-row fixnum (complex double-float))
			  (complex double-float)) set-row-ref))
(defun set-row-ref (row index value)
  (declare (type virtual-row row)
	   (type fixnum index)
	   (type (complex double-float) value))
  (nth-value 0 (funcall (virtual-row-setter row) index value)))

(defsetf row-ref set-row-ref)

(declaim (ftype (function (virtual-row)
			  (values (or virtual-row null) boolean)) next-row))
(defun next-row (row)
  (declare (type virtual-row row))
  (funcall (virtual-row-next row)))
