(ql:quickload :ltk)
(ql:quickload :zpng)
(ql:quickload :lisp-unit)

(defpackage :layers
  (:use :common-lisp
	:ltk
	:lisp-unit
	:zpng)
  (:export :add-layer
	   :remove-layer
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
	   :initialize-layer-manager))
		 

(in-package :layers)

(defparameter *layers* '(()))
(defparameter *current-layer* 0)
(defparameter *canvas* nil)

(defun create-id-generator ()
  (let ((id 0))
    (lambda ()
      (setf id (+ id 1)))))

(defparameter id-generator (create-id-generator))

(defstruct img (image)(x)(y)(height)(width)(id (funcall id-generator)))

(defun initialize-layer-manager (canvas)  
  (setf *canvas* canvas))

(defun clear-canvas ()
  (ltk:configure *canvas* :background :white))

(defun clear-layers ()
  (setf *layers* (list '()))
  (setf *current-layer* 0))

(defun draw-layers-on-canvas ()
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

(defun remove-layer (n)
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

(define-test layer-tests					
  (clear-layers)
  (let ((my-image (make-img :x 0
			    :y 10
			    :width 100
			    :height 200
			    :image 'my-image)))
    (add-image my-image)
    (assert-equal 1 (layer-count))
    (assert-equal *current-layer* 0)
    (assert-true (member my-image (current-layer)))
    (remove-image my-image)
    (assert-false (member my-image (current-layer)))
    (assert-equal 1 (layer-count))
    (add-layer)
    (add-image my-image)
    (assert-equal 1 (length (current-layer)))
    (add-layer)
    (add-image my-image)
    (assert-equal 1 (length (current-layer)))
    (assert-equal 3 (layer-count))
    (remove-layer *current-layer*)
    (assert-equal 2 (layer-count))
    (remove-layer *current-layer*)
    (assert-equal 1 (layer-count))
    (assert-true (null (current-layer)))))
