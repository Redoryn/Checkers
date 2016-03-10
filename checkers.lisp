(in-package :checkers)

(defun pawnp (piece)
  (eq 'pawn (rank piece)))

(defun kingp (piece)
  (eq 'king (rank piece)))

(defun rank (piece)
  (cond
    ((or (eq piece 'red-pawn)
	 (eq piece 'black-pawn))
     'pawn)
    ((or (eq piece 'red-king)
	 (eq piece 'black-king))
     'king)
    (t nil)))

(defun color (piece)
  (cond
    ((or (eq piece 'red-pawn)
	 (eq piece 'red-king))
	 'red)
     ((or (eq piece 'black-pawn)
	  (eq piece 'black-king))
      'black)
     (t nil)))
   

(defun fill-board (piece)
  (loop for row from 1 to 8 collect
       (loop for column from 1 to 8 collect piece)))
	           
(defun toggle-function (state-a state-b)
  (let ((current-state state-b)
	(a state-a)
	(b state-b))
    (lambda ()
      (if (eq current-state a)
	  (setf current-state b)
	  (setf current-state a))
      current-state)))

(defun initial-setup ()
  (let ((board (fill-board 'empty)))
    (setf board (place-pieces '((1 2)(1 4)(1 6)(1 8)
				(2 1)(2 3)(2 5)(2 7)
				(3 2)(3 4)(3 6)(3 8))
			      board
			      'red 'pawn))
    (setf board (place-pieces '((6 1)(6 3)(6 5)(6 7)
				(7 2)(7 4)(7 6)(7 8)
				(8 1)(8 3)(8 5)(8 7))
			      board
			      'black 'pawn))
    board))


(defun insert-at-column (row column item)
  (if (= column 1)
      (cons item (cdr row))
      (cons (car row)
	    (insert-at-column (cdr row) (- column 1) item))))

(defun place-on-board (move board p)
  (let ((c (get-column move))
	(r (get-row move)))
    (cond
      ((= r 1)(cons
	       (insert-at-column (car board) c p)
	       (cdr board)))
      (t (cons (car board)
	       (place-on-board (list (- r 1) c) (cdr board) p)))))) 

(defun remove-piece (move board)
  (let ((p (get-piece move board)))
    (values-list (list (place-on-board move board 'empty) p))))

(defun place-piece (move board color rank)
  (let ((piece 'empty))
    (cond
      ((eq 'red color)
       (if (eq 'pawn rank)
	   (setf piece 'red-pawn)
	   (setf piece 'red-king)))
      ((eq 'black color)
       (if (eq 'pawn rank)
	   (setf piece 'black-pawn)
	   (setf piece 'black-king))))
    (place-on-board move board piece)))
  
(defun place-pieces (moves board color rank)
  (let ((b (copy-board board)))
    (dolist (move moves)
      (setf b (place-piece move b color rank)))
    b))


(defun get-piece (move board)
  (let ((c (get-column move))
	(r (get-row move)))	  
    (nth (- c 1) (nth (- r 1) board))))

(defun in-boundsp (move)
  (let ((c (get-column move))
	(r (get-row move)))
    (and (>= c 1) (<= c 8)
	 (>= r 1) (<= c 8))))
  
(defun in-bounds (moves)
  (remove-if-not #'in-boundsp moves))

(defun get-neighbor-squares (move)
  (let* ((r (get-row move))
	 (c (get-column move))
	 (dn-left (list (+ r 1) (- c 1)))
	 (dn-right (list (+ r 1) (+ c 1)))
	 (up-left (list (- r 1) (- c 1)))
	 (up-right (list (- r 1) (+ c 1))))
    (in-bounds (list dn-left dn-right up-left up-right))))

(defun get-jump-landing (start-move enemy-move)
  (let ((r (+ (get-row enemy-move) (- (get-row enemy-move) (get-row start-move))))
	(c (+ (get-column enemy-move) (- (get-column enemy-move) (get-column start-move)))))
    (list r c)))

(defun get-jumped-enemy-location (start-move landing)
  (list (/ (+ (get-row start-move) (get-row landing)) 2)
	(/ (+ (get-column start-move) (get-column landing)) 2)))


(defun get-available-jumps (move board)
  (let ((neighbor-squares (get-neighbor-squares move))
	(jumps '()))
    (loop for sq in neighbor-squares do
	 (let ((landing-sq (get-jump-landing move sq)))
	   (when (valid-jump move sq landing-sq board)
	     (setf jumps (cons (list move sq landing-sq) jumps))
	     (setf jumps (append jumps
				 (mapcar
				  (lambda (jump-lst)
				    (append (list move sq) jump-lst))
				  (get-available-jumps
				   landing-sq
				   (move-by-jump move sq landing-sq board))))))))
    jumps))
				  


(defun get-available-moves (move board)
  (let ((neighbor-squares (get-neighbor-squares move))
	(available-moves '()))
    (loop for sq in neighbor-squares do
	 (if (and (emptyp sq board) (valid-moves (list move sq) board))
	     (setf available-moves (cons (list move sq) available-moves))))
    (setf available-moves (append (get-available-jumps move board)
				  available-moves))
    available-moves))
		   
		     
(defun get-row (move)
  (first move))
(defun get-column (move)
  (second move))

(defun distance (start end)
  (list (abs (- (get-row start) (get-row end)))
	(abs (- (get-column start) (get-column end)))))

(defun direction (start end)
  (let ((row-diff (- (get-row end) (get-row start)))
	(col-diff (- (get-column end) (get-column start))))
    (cond
      ((= 0 row-diff) 'same-row)
      ((= 0 col-diff) 'same-column)
      ((> row-diff 0) 'down)
      (t 'up))))

(defun valid-distancep (start end)
  (let ((d (distance start end)))
    (= (first d)(second d) 1)))
	  
(defun get-color-from-move(move board)
  (color (get-piece move board)))

(defun get-side-from-move (move board)
  (get-color-from-move move board))

(defun valid-directionp (piece direction)
  (cond
    ((eq 'empty piece) nil)
    ((kingp piece) t)
    ((pawnp piece)
     (if (eq (color piece) 'red)
	 (eq direction 'down)
	 (eq direction 'up)))
    (t nil)))

(defun enemyp (p1 p2)
  (not (eq (color p1) (color p2))))

(defun enemy-color (color)
  (if (eq color 'red) 'black 'red))

(defun empty-space (move board)
  (emptyp move board))

(defun emptyp (move board)
  (eq (get-piece move board) 'empty))

(defun valid-jump (start middle end board)
  (when (and (in-boundsp start)
	     (in-boundsp middle)
	     (in-boundsp end)
	     (not (emptyp start board))
	     (not (emptyp middle board))
	     (emptyp end board))
  
    (and (valid-directionp (get-piece start board)
			   (direction start middle))
	 (eq (direction start middle) (direction middle end))
	 (and (= 1 (first (distance start middle)))
	      (= 1 (second (distance start middle))))
	 (and (= 1 (first (distance  middle end)))
	      (= 1 (second (distance middle end))))
	 (empty-space end board)
	 (eq (enemy-color (get-side-from-move start board))
	     (get-side-from-move middle board)))))

(defun move-to-empty-space (start end board)
  (multiple-value-bind (board p) (remove-piece start board)
    (place-on-board end board p)))
  
(defun move-by-jump (start middle end board)
  (multiple-value-bind (board p) (remove-piece start board)
    (place-on-board end (remove-piece middle board) p)))

(defun copy-board (board)
  (loop for row in board collect
       (copy-list row)))
       
(defun valid-moves (moves board)
  (let ((p (get-piece (first moves) board))
	(temp-board (copy-board board)))
    (labels ((validate (moves board in-jump)
	       (let ((number-of-moves (length moves)))
		 (cond
		   ((null moves) board)
		   ((and (= 1 number-of-moves) in-jump) board)
		   ((and (= 2 number-of-moves)
			 (equal '(2 2) (distance (first moves) (second moves))))
		    (validate (cons (car moves)
				    (cons (get-jumped-enemy-location (first moves) (second moves))
					  (cdr moves)))
			      board
			      in-jump))
		   ((and (= 2 number-of-moves)
			 (empty-space (second moves) board)
			 (valid-directionp p (direction (first moves) (second moves)))
			 (valid-distancep (first moves) (second moves))
			 (null (cddr moves)))
		    (move-to-empty-space (first moves) (second moves) board))
		   ((and (>= number-of-moves 3)
			 (not (emptyp (second moves) board))
			 (enemyp p (get-piece (second moves) board))
			 (valid-jump (first moves) (second moves) (third moves) board))
		    (validate (cddr moves)
			      (move-by-jump (first moves)
					    (second moves)
					    (third moves)
					    board)
			      t))
		   (t temp-board)))))		    		      
      (let ((new-board (validate moves board nil)))
	(values-list (list (not (equal-boards temp-board new-board))
			  new-board))))))

(defun equal-boards (b0 b1)
  (equal b0 b1))

(defun score-board (board)
  "Returns numbers of pieces per side on board -> '(n-red n-black)"
  (let ((red 0)
	(black 0))
    (dolist (row board)
      (dolist (p row)
	(cond
	  ((eq p 'empty) 0)
	  ((eq (color p) 'red) (incf red))
	  ((eq (color p) 'black) (incf black))
	  (t 0))))
    (list red black)))
			      
	 	   
(defun double-jump()
    (let ((board (fill-board 'empty)))
    (setf board (place-piece '(1 1) board 'red 'pawn))
    (setf board (place-piece '(2 2) board 'black 'pawn))
    (setf board (place-piece '(4 4) board 'black 'pawn))
    (print-board board)
    
    (multiple-value-bind (validp new-board) (valid-moves '((1 1)(2 2)(3 3)(4 4)(5 5)) board)
      (if validp (print-board new-board)))))



(defun update-board-with-kings (board)
  (loop for n from 1 to 8 do
       (let ((top-move (list 1 n))
	     (bottom-move (list 8 n)))
	 (if (and (not (emptyp top-move board))
		  (eq 'black (color (get-piece top-move board))))
	     (setf board (place-piece top-move board 'black 'king)))
	 (if (and (not (emptyp bottom-move board))
		  (eq 'red (color (get-piece bottom-move board))))
	     (setf board (place-piece bottom-move board 'red 'king)))))
  board)
       	
  

(defun print-board (board)
  (terpri)
  (let ((checker #\Space))
    (dolist (row board)
      (dolist (p row)	
	(cond
	  ((eq p 'empty) (setf checker #\Space))
	  ((eq (color p) 'red)
	   (if (kingp p)
	       (setf checker #\R)
	       (setf checker #\r)))
	  ((eq (color p) 'black)
	   (if (kingp p)
	       (setf checker #\B)
	       (setf checker #\b)))
	  (t (setf checker #\Space)))
	(princ checker)
	(princ #\Space))
      (fresh-line))))
	
(defun game-overp (board)
  (let ((score (score-board board)))
    (or (zerop (first score))
	(zerop (second score))
	(null (get-all-possible-moves 'red board))
	(null (get-all-possible-moves 'black board))
	)))

(defun declare-winner (board)
  (let* ((score (score-board board))
	 (winner (if (zerop (first score)) 'black 'red)))  ;fix bug where red wins if black can't move
    (format t "Winner! - ~a - Congratulations!!~%" winner)
    (format t "Final Score - Red: ~a Black: ~a~%" (first score) (second score))))

(defparameter *board-state-history* '())
(defparameter *keep-history* nil)

(defun new-game (red-player-f black-player-f update-board-f quit-f)
  (let* ((board (initial-setup))	 
	 (next-player (toggle-function red-player-f black-player-f))
	 (next-color (toggle-function 'red 'black))
	 (current-player (funcall next-player))
	 (current-color  (funcall next-color)))
    (setf *board-state-history* (list board))
    (loop while (and (not (game-overp board))
		     (funcall quit-f))
       do
	 (progn
	   (funcall update-board-f board)
	   (let ((moves (funcall current-player board)))
	     (format t "Selected Moves: ~a~%" moves)
	     (when (and (not (emptyp (car moves) board))
			(eq current-color (get-color-from-move (car moves) board)))
	       (multiple-value-bind (is-valid new-board) (valid-moves moves board)
		 (when is-valid		   
		   (setf board (update-board-with-kings new-board))
		   (if *keep-history*
		       (setf *board-state-history* (cons board *board-state-history*)))
		   (setf current-color (funcall next-color))
		   (setf current-player (funcall next-player))))))))
    (funcall update-board-f board)
    (declare-winner board)
    ))

(defun walk-game (states display-fn)
  (let ((cursor 0)
	(max-cursor (length states))
	(history states))
    (lambda (direction)
      (cond
	((and (eq direction 'forward)	 
	      (> cursor 0))
	 (setf cursor (- cursor 1)))
	((and (eq direction 'backward)
	      (< cursor max-cursor))
	 (setf cursor (+ 1 cursor))))
      ;(format t "Cursor: ~A~%" cursor)
      (funcall display-fn (nth cursor history)))))
      
      
	      
	       


(defun move-from-console (color)
  (format t "~a:> " color)
  (let ((dirty-input (read-line)))
    (read-from-string (concatenate 'string "(" dirty-input ")"))))


(defun my-game()
  (let ((red-player (lambda () (move-from-console 'red)))
	(black-player (lambda () (move-from-console 'black)))
	(update-board (lambda (board) (print-board board)))
	(quit-f (lambda () t)))
    (new-game red-player black-player update-board quit-f)))
    
	       
		 
  
