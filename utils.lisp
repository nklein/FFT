
(in-package #:fft)

(deftype small-double-float ()
  '(double-float #.(- pi) #.pi))

(deftype length-type ()
  '(and fixnum unsigned-byte))

(defvar *empty-array* (make-array '(1)
				  :element-type '(complex double-float)
				  :initial-element (complex 0.0d0 0.0d0)))

(defmacro vr-compile (form)
  #+(or :abcl :clisp :cmu) form
  #-(or :abcl :clisp :cmu) `(compile nil ,form))

(defstruct virtual-row
  (length 0 :type length-type)
  (buffer *empty-array* :type (simple-array (complex double-float) *))
  (offset 0 :type length-type)
  (span 0 :type length-type)
  next)

(defun vr-offset (buffer pre post &optional (index 0))
  (apply #'array-row-major-index buffer (append pre (list index) post)))

(defun vr-span (buffer pre post)
  (- (vr-offset buffer pre post
		(min 1 (1- (array-dimension buffer (length pre)))))
     (vr-offset buffer pre post)))

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
		      :buffer buffer
		      :offset (vr-offset buffer pre post)
		      :span   (vr-span buffer pre post)
		      :next   (vr-compile
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
			  (complex double-float)) row-ref)
	 (inline row-ref))
(defun row-ref (row index)
  (declare (type virtual-row row)
	   (type length-type index)
	   (optimize (speed 3)))
  (let ((buffer (virtual-row-buffer row))
	(span   (virtual-row-span row))
	(offset (virtual-row-offset row)))
    (declare (type (simple-array (complex double-float) *) buffer)
	     (type length-type span offset))
  (let ((major (the length-type (* index span))))
    (declare (type length-type major))
    (the (complex double-float)
      (row-major-aref buffer
		      (the length-type (+ major offset)))))))

(declaim (ftype (function (virtual-row length-type (complex double-float))
			  (complex double-float)) set-row-ref)
	 (inline set-row-ref))
(defun set-row-ref (row index value)
  (declare (type virtual-row row)
	   (type length-type index)
	   (type (complex double-float) value))
  (setf (row-major-aref (virtual-row-buffer row)
			(+ (* index (virtual-row-span row))
			   (virtual-row-offset row))) value))

(defsetf row-ref set-row-ref)

(declaim (ftype (function (virtual-row)
			  (values (or virtual-row null) boolean)) next-row))
(defun next-row (row)
  (declare (type virtual-row row))
  (funcall (virtual-row-next row)))
