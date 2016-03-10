(ql:quickload :ltk)
(ql:quickload :zpng)
(ql:quickload :png-read)
(ql:quickload :lisp-unit)

(defpackage :layers
  (:use :common-lisp
	:ltk
	:lisp-unit
	:zpng)	
  (:export :add-layer
	   :remove-layer
	   :clear-layer
	   :clear-layers
	   :move-up-layer
	   :move-down-layer
	   :set-current-layer
	   :img
	   :make-img
	   :img-x
	   :img-y
	   :img-width
	   :img-height
	   :img-image
	   :img-id
	   :add-image	   
	   :remove-image
	   :draw-layers-on-canvas
	   :get-master-canvas
	   :master-layer-refresh
	   :initialize-layer-manager))
		 

(in-package :layers)

(defparameter *layers* '(()))
(defparameter *current-layer* 0)
(defparameter *canvas* nil)
(defparameter *canvas-size* 750)

(defun get-master-canvas ()
  *canvas*)

(defun create-id-generator ()
  (let ((id 0))
    (lambda ()
      (setf id (+ id 1)))))

(defparameter id-generator (create-id-generator))

(defstruct img (image)(x)(y)(height)(width)(id (funcall id-generator)))

(defun initialize-layer-manager (canvas)  
  (setf *canvas* canvas))

(defun clear-canvas ()
  (clear *canvas*))

(defun clear-layers ()
  (setf *layers* (list '()))
  (setf *current-layer* 0))

(defun clear-layer ()
  (setf *layers* (cons '() (cdr *layers*))))

(defparameter *master-layer-image* nil)

(defun draw-layers-on-canvas ()
  (let* ((canvas *canvas*)
	 (layers *layers*)
	 (pic (make-instance 'png :width *canvas-size* :height *canvas-size* :color-type :truecolor-alpha))
	 (pic-data (data-array pic))
	 (ltk-image (make-image)))
    (dolist (layer (reverse layers))
      (dolist (img (reverse layer))
	(insert-png pic-data
		    (png-read:image-data (img-image img))
		    (img-x img)
		    (img-y img))))
		    						
    (write-png pic output-png)
    (image-load ltk-image output-png)
    (setf *master-layer-image* ltk-image)
    (ltk:create-image *canvas* 0 0 :image ltk-image)))



(defun master-layer-refresh ()
  (ltk:create-image *canvas* 0 0 :image *master-layer-image*))

(defparameter output-png  "~/Documents/lisp/Checkers/board-layout.png")

(defun emptyp-pixel (data x y)
  (and (zerop (aref data x y 0))
       (zerop (aref data x y 1))
       (zerop (aref data x y 2))
       (zerop (aref data x y 3))))


(defun insert-png (src item x y)
  (loop for py from 0 to (- (array-dimension item 0) 1) do
       (loop for px from 0 to (- (array-dimension item 1) 1) do
	    (when (not (emptyp-pixel item px py))
	      (loop for i from 0 to (- (array-dimension item 2) 1) do
		   (setf (aref src (+ y py) (+ x px) i) (aref item px py i)))))))
		      

(defun draw-layers-on-canvas-ltk ()
  (let ((canvas *canvas*)
	(layers *layers*))
    (clear-canvas)
    (dolist (layer (reverse layers))
      (dolist (img (reverse layer))
	(ltk:create-image canvas (img-x img) (img-y img)
			  :image (img-image img))))))

(defun add-new-layer ()
  (new-layer))
	
(defun add-layer ()
  (new-layer))

(defun new-layer ()
  (setf *current-layer* (+ *current-layer* 1))
  (setf *layers* (cons '() *layers*)))

(defun remove-layer (&key (n *current-layer*))
  (labels ((r (layers n)
	     (if (= n 0)
		 (cdr layers)
		 (cons (car layers)
		       (r (cdr layers) (- n 1))))))
    (setf *current-layer* (- *current-layer* 1))
    (setf *layers* (r *layers* (n-to-index n)))))
    
(defun n-to-index (n)
  (- (layer-count) n 1))
    
(defun remove-image (img)
  (insert-layer (remove img (current-layer) :key #'img-id :test-not #'eq) *current-layer*))

(defun layer-count ()
  (length *layers*))

(defun insert-layer (layer n)
  (setf (nth n *layers*) layer))
      
(defun current-layer()
  (car *layers*))

(defun add-image (img)
  (insert-layer (cons img (current-layer)) (n-to-index *current-layer*)))

(defun move-up-layer ()
  (if (/= *current-layer* (layer-count))
      (setf *current-layer* (+ 1 *current-layer*))))

(defun move-down-layer ()
  (if (/= *current-layer* 0)
      (setf *current-layer* (- *current-layer* 1))))

(defun set-current-layer (n)
  (if (and (>= n 0)
	   (< n (layer-count)))
      (setf *current-layer* n)))

(defun show-layers ()
  *layers*)

(defun test-image ()
  (make-img :x 0
	    :y 10
	    :width 100
	    :height 200
	    :image 'my-image))


