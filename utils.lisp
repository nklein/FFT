
(in-package #:fft)

(defstruct virtual-row
  length
  getter
  setter
  next)

(defun vr-aref (buffer pre index post)
  (eval `(aref ,buffer ,@pre ,index ,@post)))

(defun vr-setf-aref (buffer pre index post value)
  (eval `(setf (aref ,buffer ,@pre ,index ,@post) ,value)))

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
		 (new-pre  (values new-pre post))
		 (new-post (values (mapcar (constantly 0) pre)
				   new-post))
		 ((<= (length (array-dimensions buffer))
		      (1+ (length pre))) (values nil nil))
		 (t (values (cons 0 (mapcar (constantly 0) pre))
			    (mapcar (constantly 0) (cdr post))))))))
    (inc buffer (copy-seq pre) (copy-seq post))))

(defun virtual-row (buffer &optional (dimension 0) pre post)
  (let ((pre  (or pre
		  (mapcar (constantly 0)
			  (subseq (array-dimensions buffer) 0 dimension))))
	(post (or post
		  (mapcar (constantly 0)
			  (subseq (array-dimensions buffer) (1+ dimension))))))
    (make-virtual-row :length (array-dimension buffer dimension)
		      :getter (lambda (index)
				(vr-aref buffer pre index post))
		      :setter (lambda (index val)
				(vr-setf-aref buffer pre index post val))
		      :next   (lambda ()
				(multiple-value-bind (new-pre new-post)
				    (vr-next-row buffer pre post)
				  (when (or new-pre new-post)
				    (virtual-row buffer
						 (length new-pre)
						 new-pre
						 new-post)))))))

(defun row-length (row)
  (virtual-row-length row))

(defun row-ref (row index)
  (with-slots (getter) row
    (funcall getter index)))

(defun set-row-ref (row index value)
  (with-slots (setter) row
    (funcall setter index value)))

(defsetf row-ref set-row-ref)

(defun next-row (row)
  (with-slots (next) row
    (funcall next)))
