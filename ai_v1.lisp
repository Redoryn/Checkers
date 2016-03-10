(in-package :checkers)

;a play is a list of moves starting with current-location and ending in blank spot
(defun score-play (move)
  (length move))

(defun make-play (moves board)
  (multiple-value-bind (is-valid new-board) (valid-moves moves board)
    new-board))


(defun look-ahead-one (board color multiplier)
  (let* ((moves (get-all-possible-moves color board))
	 (max-moves nil)
	 (max-score -99))
    (loop for move in moves do
	 (let* ((enemy-score (score-play (car (get-max-moves 
					       (get-all-possible-moves
						(enemy-color color)
						(make-play move board))
					       #'score-play))))
		  (move-score (+ (score-play move) (* -1 enemy-score))))
	   (cond
	     ((> move-score max-score)
	      (progn
		(setf max-score move-score)
		(setf max-moves (list move))))
	     ((= move-score max-score)
	      (setf max-moves (cons move max-moves))))))
    (format t "Max move: ~a~%" max-moves)
    max-moves))
	 
(defun get-nearest-pieces (board position)
  (let ((min-d 100)
	(nearest-enemies '())	
	(color (color (get-piece position board))))
    (loop for enemy-pos in (get-all-occupied-locations board :color (enemy-color color)) do
	 (let ((d (distance position enemy-pos)))
	   (when (<= d min-d)
	     (if (< d min-d)
		 (setf nearest-enemies '()))
	     (push enemy-pos nearest-enemies))))
    nearest-enemies))
		 


(defun get-all-occupied-locations (board &key (color 'any))
  (loop for r from 1 to 8 append
       (loop for c from 1 to 8
	  when (and (not (emptyp (list r c) board))
		    (or (eq color 'any)
			(eq color (color (get-piece (list r c) board)))))
	  collect (list r c))))
		     
       	     
(defun get-all-possible-moves (color board)
  (loop for move in (get-all-occupied-locations board :color color)
       append (get-available-moves move board)))

(defun get-max-moves (moves fn)
  (let ((max (funcall fn (car moves)))
	(mvs (list (car moves))))
    (loop for move in (cdr moves) do
	 (cond
	   ((> (funcall fn move) max)
	    (progn
	      (setf mvs (list move))
	      (setf max (funcall fn move))))
	   ((= (funcall fn move) max)
	    (setf mvs (cons move mvs)))))
    mvs))
	 

(defun get-moves-by-length (moves length)
  (cond
    ((null moves) '())
    ((= (length (car moves)) length)
     (cons (car moves)
	   (get-moves-by-length (cdr moves) length)))
    (t (get-moves-by-length (cdr moves) length))))

(defun random-elt (lst)
  (nth (random (length lst)) lst))


(defun play-longest-move (color board)
  (let* ((moves (get-all-possible-moves color board))
	 (max-move-length (get-max-move moves))
	 (max-options (get-moves-by-length moves max-move-length)))
    (format t "Max move length: ~a~%" max-move-length)
    (format t "Max options: ~a~%" max-options)
    (random-elt max-options)))

(defun play (color board)
  (random-elt (look-ahead-one board color 1)))
    

