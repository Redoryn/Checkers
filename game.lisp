(ql:quickload :png-read)
(ql:quickload :ltk)
(ql:quickload :lisp-unit)

(load "~/Documents/lisp/Checkers/layers.lisp")

(defpackage :checkers
  (:use :common-lisp
	:ltk	
	:png-read
	:lisp-unit
	:layers)
  (:import-from :png-read :read-png-file))


(load "~/Documents/lisp/Checkers/checkers.lisp")
(load "~/Documents/lisp/Checkers/ai_v1.lisp")
(load "~/Documents/lisp/Checkers/unit_tests.lisp")

(in-package :checkers)

(defparameter *square-size* 80)


(defparameter *image-filepaths*
  '((black-pawn . "~/Documents/lisp/Checkers/black-resized.png")
    (black-king . "~/Documents/lisp/Checkers/black-king-resized.png")
    (red-pawn . "~/Documents/lisp/Checkers/yellow-resized.png")
    (red-king . "~/Documents/lisp/Checkers/yellow-king-resized.png")
    (tile-highlight . "~/Documents/lisp/Checkers/tile-highlight.png")
    (board . "~/Documents/lisp/Checkers/checkerboard_bare.png")
    (tile-a . "~/Documents/lisp/Checkers/tile_a.png")
    (tile-b . "~/Documents/lisp/Checkers/tile_b.png")))
    
(defun load-image-assets ()
  (loop for pair in *image-filepaths*
     collect (let ((img (make-image))
		   (img-name (car pair))
		   (img-path (cdr pair)))
	       (image-load img img-path)
	       `(,img-name . ,img))))

(defparameter *image-assets* nil)

(defun get-image (name)
  (cdr (assoc name *image-assets*)))


(defparameter *png-assets* nil)

(defun load-png-assets ()
  (loop for pair in *image-filepaths*
     collect (let ((img-name (car pair))
		   (img-path (cdr pair)))
	       (cons img-name (read-png-file img-path)))))

(defun get-png (name)
  (cdr (assoc name *png-assets*)))



(defun update-board (board)
  (let ((x *x-offset*)(y *y-offset*)(pieces '()))
    (dolist (row board)
      (dolist (checker row)
	(when (or (eq checker 'red-pawn) (eq checker 'black-pawn)
		  (eq checker 'red-king) (eq checker 'black-king))
	  (setf pieces
		(cons (make-img :image (get-png checker)
				:x x
				:y y
				:height 76
				:width 76) pieces)))
	(setf x (+ x *tile-size*)))
      (setf x *x-offset*)
      (setf y (+ y *tile-size*)))
    pieces))
	  

(defparameter row-lookup
  '((a . 1)
    (b . 2)
    (c . 3)
    (d . 4)
    (e . 5)
    (f . 6)
    (g . 7)
    (h . 8)))

(defun coord->move(coord)
  (let ((column (cdr (assoc (first coord) row-lookup)))
	(row (+ 1 (abs (- (second coord) 8)))))
    (list row column)))

(defun coords->moves(coords)
  (mapcar #'coord->move coords))

(defun on-update(board)
  (clear-layer)
  (dolist (checker (update-board board))
    (add-image checker))
  (draw-layers-on-canvas))
  
(defun on-update-at-end (board)
  (if (game-overp board)
      (on-update board)))

(defun play-script (script)
  (let ((moves script))
    (lambda (b)
      (let ((next-move (car moves)))
	(setf moves (cdr moves))
	(sleep .7)
	(if (null moves)
	    (setf *continue-playing* nil))
	(coords->moves next-move)))))
	    
(defun tile-click->move (evt)
  (let* ((tile-size *tile-size*)
	 (x (- (event-x evt) *x-offset*))
	 (y (- (event-y evt) *y-offset*))
	 (column (+ 1 (floor (/ x tile-size))))
	 (row (+ 1 (floor (/ y tile-size)))))
    (if (and (>= row 1) (<= row 8)
	     (>= column 1) (<= column 8))
	(list row column)
	nil)))

(defun highlight-tile (row column)
  (let* ((tile-size *tile-size*)
	 (x (+ *x-offset* (* tile-size (- column 1))))
	 (y (+ *y-offset* (* tile-size (- row 1))))
	 (img (make-image)))
    (create-image (get-master-canvas) x y :image (get-image 'tile-highlight))))

(defun highlight-tiles (moves)
  (dolist (m moves)
    (highlight-tile (first m) (second m))))

(defun clear-highlights ()
  (master-layer-refresh))
  

(defun move-from-mouse ()
  (let ((moves '()))
    (lambda (state evt)
      (let ((new-move (tile-click->move evt)))      
	(when (and (or (eq state 'add) (eq state 'turn))
		   new-move)	       
	  (setf moves (remove-duplicates (cons new-move moves) :test 'equal))
	  (highlight-tiles moves))
	;(format t "Moves: ~a ~%" moves)
	(cond
	  ((eq state 'remove)
	   (progn
	     (setf moves '())
	     (clear-highlights)))
	  ((eq state 'turn)
	   (progn	   
	     (setf *player-moves* (reverse moves))
	     (setf moves '()))))))))	
	   
(defun next-player (p)
  (if (eq p 'red) 'black 'red))

(defun player-turn (color)
  (if (not (eq color *current-player*))
      (setf *current-player* color))
  (format t "~A:> " *current-player*)
  (loop while (null *player-moves*)  do
       (process-events))
  (let ((moves-to-return *player-moves*))
    (setf *player-moves* '())
    moves-to-return))

(defparameter *tile-size* 76)
(defparameter *x-offset* 74)
(defparameter *y-offset* 52)
(defparameter *player-moves* '())
(defparameter *current-player* nil)
(defparameter *continue-playing* t)	

(defun add-tiles ()
  (let* ((t0 (get-png 'tile-a))
	 (t1 (get-png 'tile-b))
	 (x *x-offset*)
	 (y *y-offset*)
	 (next-line (toggle-function (list t0 t1 t0 t1 t0 t1 t0 t1)
				     (list t1 t0 t1 t0 t1 t0 t1 t0))))
    (dotimes (n 8)
      (let ((line (funcall next-line)))
	(dolist (img line)
	  (add-image (make-img :image img
			       :x x
			       :y y
			       :height *square-size*
			       :width *square-size*))
	  (setf x (+ x *tile-size*)))
	(setf x *x-offset*)
	(setf y (+ y *tile-size*))))))
	  
       

(defun main ()
  (with-ltk ()
    (format-wish "package require Img")
    (setf *image-assets* (load-image-assets))
    (setf *png-assets* (load-png-assets))
    (setf *continue-playing* t)
    (setf *current-player* 'red)
    (setf *player-moves* '())
    (clear-layers)
    (let* ((frm (make-instance 'frame))
           (c (make-instance 'canvas :height 750 :width 750))
           (layer-manager (layers:initialize-layer-manager c))
	   (board (make-img :image (get-png 'board) :x 0 :y 0
			    :height 100 :width 100))
	   (game-history (walk-game *board-state-history* #'on-update))
	   (quit-f (lambda () *continue-playing*))
	   (scripted-game (play-script game-script))
	   (player-move-f (move-from-mouse))
	   ;(p1 (lambda(b) (player-turn 'red)))
	   (p1-computer (lambda (b)(sleep .1)(play 'red b)))
	   (p2-computer (lambda (b)(sleep .1)(play 'black b)))
	   (p2 (lambda() (player-turn 'black))))
	   ;(p1 (lambda () (coords->moves (move-from-console 'red))))
	   ;(p2 (lambda () (coords->moves (move-from-console 'black)))))

      (focus c)
      (bind c "<ButtonPress-1>" (lambda (evt)(funcall player-move-f 'add evt)))
      (bind c "<ButtonPress-2>" (lambda (evt)(funcall player-move-f 'remove evt)))
      (bind c "<ButtonPress-3>" (lambda (evt)(funcall player-move-f 'turn evt)))
      (bind c "<KeyPress-Return>"
	    (lambda (evt)(setf game-history (walk-game *board-state-history* #'on-update))))
      (bind c "<KeyPress-Up>" (lambda (evt) (funcall game-history 'forward)))
      (bind c "<KeyPress-Down>" (lambda (evt) (funcall game-history 'backward)))      
      (add-image board)
      (add-tiles)
      (add-layer)
      (pack frm)
      (pack c)
      (draw-layers-on-canvas)
      (new-game p1-computer p2-computer #'on-update quit-f)
      ;(new-game p1 p2-computer #'on-update quit-f)
      ;(new-game scripted-game scripted-game #'on-update quit-f)
      )))



(defparameter game-script
  '(((b 6)(c 5))
    ((c 3)(b 4))
    ((d 6)(e 5))
    ((b 4)(c 5)(d 6))
    ((e 7)(d 6)(c 5))
    ((d 2)(c 3))
    ((c 7)(b 6))
    ((e 3)(d 4))
    ((c 5)(d 4)(e 3))
    ((f 2)(e 3)(d 4))))
