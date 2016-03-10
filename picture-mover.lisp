(ql:quickload :ltk)
(ql:quickload :zpng)

(defpackage :image-helper
  (:use :common-lisp
	:ltk
	:zpng)
  (:export :image-mover))

(in-package :image-helper)

(defun image-mover (canvas img x y)
  (let* ((img-item (create-image canvas x y :image img))
	 (canvas canvas)
	 (x-step-size 10)
	 (y-step-size 5)
	 (x x)
	 (y y)	 
	 (tl (make-instance 'toplevel))
	 (fr (make-instance 'frame :master tl))
	 (btn-up (make-instance 'button
				:text "Up"
				:master fr))
	 (btn-down (make-instance 'button
				  :text "Down"
				  :master fr))
	 (btn-left (make-instance 'button
				  :text "Left"
				  :master fr))
	 (btn-right (make-instance 'button
				   :text "Right"
				   :master fr)))
    (pack fr)
    (pack btn-up :side :top)
    (pack btn-left :side :left)
    (pack btn-right :side :right)
    (pack btn-down :side :bottom)
    (wm-title tl (format nil "(~a,~a)" x y))
    (setf fn-update
	  (lambda(direction)
	    (progn
	      (cond
		((eq direction 'left) (setf x (- x x-step-size)))
		((eq direction 'right) (setf x (+ x x-step-size)))
		((eq direction 'down) (setf y (+ y y-step-size)))
		((eq direction 'up) (setf y (- y y-step-size))))
	      (set-coords canvas img-item (list x y))
	      (wm-title tl (format nil "(~a,~a)" x y)))))
    (setf (command btn-up) (lambda() (funcall fn-update 'up)))
    (setf (command btn-down) (lambda() (funcall fn-update 'down)))
    (setf (command btn-left) (lambda() (funcall fn-update 'left)))
    (setf (command btn-right) (lambda() (funcall fn-update 'right)))))
