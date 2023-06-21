(defvar *board* (make-array '(3 3)))

(defun tile-char (tile)
    (ecase tile
        (0 #\-) ;; 0 is empty
        (1 #\O) ;; 1 is nought
        (2 #\X) ;; 2 is cross
    )
)

(defun print-board ()
    (dotimes (i (array-dimension *board* 0))
        (dotimes (j (array-dimension *board* 1))
            (format t "~A " (tile-char (aref *board* j i)))
        )
        (format t "~%")
    )
)

(defun check-win ()
    (let ((win-tile 0))
        ;; check horizontal
        (dotimes (i (array-dimension *board* 1))
            (if (and
                    (= (aref *board* 0 i) (aref *board* 1 i))
                    (= (aref *board* 0 i) (aref *board* 2 i)))
                (let ((tile 0))
                    (setf tile (aref *board* 0 i))
                    (if (not (= tile 0))
                        (progn
                            (setf win-tile tile)
                            (return)
                        )
                    )
                )
            )
        )
        
        ;; check vertical
        (if (= win-tile 0)
            (dotimes (i (array-dimension *board* 0))
                (if (and
                        (= (aref *board* i 0) (aref *board* i 1))
                        (= (aref *board* i 0) (aref *board* i 2)))
                    (let ((tile 0))
                        (setf tile (aref *board* i 0))
                        (if (not (= tile 0))
                            (progn
                                (setf win-tile tile)
                                (return)
                            )
                        )
                    )
                )
            )
        )
        
        ;; check diagonal
        (if (= win-tile 0)
            (if (and
                    (= (aref *board* 0 0) (aref *board* 1 1))
                    (= (aref *board* 0 0) (aref *board* 2 2)))
                (let ((tile 0))
                    (setf tile (aref *board* 0 0))
                    (if (not (= tile 0))
                        (setf win-tile tile)
                    )
                )
                (if (and
                        (= (aref *board* 2 0) (aref *board* 1 1))
                        (= (aref *board* 2 0) (aref *board* 0 2)))
                    (let ((tile 0))
                        (setf tile (aref *board* 2 0))
                        (if (not (= tile 0))
                            (setf win-tile tile)
                        )
                    )
                )
            )
        )

        (if (not (= win-tile 0))
            (progn
                (print-board)
                (format t "~%~A wins!~%" (tile-char win-tile))
                (exit)
            )
        )
    )
)

(defun computer-move ()
    (let (
            (x 0)
            (y 0)
        )
        (loop
            (setf x (random (array-dimension *board* 0)))
            (setf y (random (array-dimension *board* 1)))
            (if (= (aref *board* x y) 0)
                (progn
                    (setf (aref *board* x y) 1)
                    (return)
                )
            )
        )
    )
)
        
(let (
        (x 0)
        (y 0)
        (ch #\X)
        (win-tile 0)
    )
    
    ;; init random state
    (setf *random-state* (make-random-state t))
    (format t "LispTicTacToe~%")
    
    (loop
        (print-board)
        
        ;; get X position
        (loop
            (format *query-io* "Enter tile X (1-3): ")
            (finish-output *query-io*)
            (setq x (parse-integer (read-line *query-io*)))
            
            ;; check if x is valid
            (if (or (< x 1) (> x 3))
                (format t "Invalid number~%")
                (return) ;; x is valid
            )
        )
        
        ;; get Y position
        (loop
            (format *query-io* "Enter tile Y (1-3): ")
            (finish-output *query-io*)
            (setq y (parse-integer (read-line *query-io*)))
            
            ;; check if y is valid
            (if (or (< x 1) (> x 3))
                (format t "Invalid number~%")
                (return) ;; y is valid
            )
        )
        
        ;; adjust x and y
        (decf x)
        (decf y)
        
        ;; set tile
        (setf (aref *board* x y) 2)
        
        ;; check win
        (check-win)
        
        ;; now its time for the computer to make a move
        (computer-move)
        
        ;; check win
        (check-win)
    )
)
