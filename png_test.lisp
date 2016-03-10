(ql:quickload :zpng)
(ql:quickload :ltk)
(ql:quickload :png-read)

(defpackage :png-fun
  (:use :common-lisp
	:zpng
	:ltk
	:png-read))

(in-package :png-fun)


(defparameter tile-a  "~/Documents/lisp/Checkers/tile_a.png")
(defparameter tile-b  "~/Documents/lisp/Checkers/tile_b.png")
(defparameter output-png  "~/Documents/lisp/Checkers/test_output.png")

(defparameter *image-filepaths*
  '((black-pawn . "~/Documents/lisp/Checkers/black-resized.png")
    (black-king . "~/Documents/lisp/Checkers/black-king-resized.png")
    (red-pawn . "~/Documents/lisp/Checkers/yellow-resized.png")
    (red-king . "~/Documents/lisp/Checkers/yellow-king-resized.png")
    (tile-highlight . "~/Documents/lisp/Checkers/tile-highlight.png")
    (board . "~/Documents/lisp/Checkers/checkerboard_bare.png")
    (tile-a . "~/Documents/lisp/Checkers/tile_a.png")
    (tile-b . "~/Documents/lisp/Checkers/tile_b.png")))
    


(defparameter *png-assets* nil)

(defun load-png-assets ()
  (loop for pair in *image-filepaths*
     collect (let ((img-name (car pair))
		   (img-path (cdr pair)))
	       (cons img-name (read-png-file img-path)))))

(defun get-png (name)
  (cdr (assoc name *png-assets*)))


(defparameter my-test '())
(defun test-draw ()
  (let* ((pic (make-instance 'png :height 200 :width 200 :color-type :truecolor))
	 (pic-data (data-array pic))
	 (pic-1 (get-image 'black-pawn))
	 (pic-2 (read-png-file tile-b)))
    (setf my-test pic-data)
    (insert-png my-test (image-data pic-1) 50 50)
    (insert-png my-test (image-data pic-2) 75 75)
    (write-png pic output-png)))


(defun insert-png (src item x y)
  (loop for px from 0 to (- (array-dimension item 0) 1) do
       (loop for py from 0 to (- (array-dimension item 1) 1) do
	    (loop for i from 0 to 2 do
		 (setf (aref src (+ x px) (+ y py) i) (aref item px py i))))))
					      
	
	

