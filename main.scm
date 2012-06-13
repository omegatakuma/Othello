#!/usr/local/bin/gosh

(define turn 1)
(define *board* (make-vector 64 'empty))
(define (board-ref n)(vector-ref *board* n))
(define (board-set! n m)(vector-set! *board* n m))

(define (jadge? color n)
  (map
	(lambda(x)
	  (board-set! x color))
	(append
	  (solve color n 1)
	  (solve color n 7)
	  (solve color n 8)
	  (solve color n 9))))

(define (solve color n m)
  (let* ((re+ (postv? (+ n m)))
		 (re- (postv? (- n m)))
		 (color2 (color-change color)))
	(if (and re+ re-)
	  (cond 
		((eq? (board-ref re+) color2)
		 (let loop ((lst (cons re+ '())))
		   (cond
			 ((or (negative? (+ (car lst) m)) (> (car lst) 64)) '())			 
			 ((eq? (board-ref (+ (car lst) m)) color)
			  lst)
			 ((eq? (board-ref (+ (car lst) m)) 'empty)
			  '())
			 (else 
			   (loop (cons (+ (car lst) m) lst))))))
		((eq? (board-ref re-) color2)
		 (let loop ((lst (cons re- '())))
		   (cond 
			 ((or (negative? (- (car lst) m)) (> (car lst) 64)) '())			 
			 ((eq? (board-ref (- (car lst) m)) color)
			  lst)
			 ((eq? (board-ref (- (car lst) m)) 'empty)
			  '())
			 (else 
			   (loop (cons (- (car lst) m) lst))))))
		(else 
		  '()))
	  '())))

(define (color-change color)
  (if (eq? color 'black)
	'white
	'black))

(define (postv? n)
  (if (and (positive? n) (< n 64)) n #f))

(define (position key lst)
  (let loop ((lst lst)(n 0)(result '()))
	(cond
	  ((null? lst) (reverse result))
	  ((eq? key (car lst))(loop (cdr lst) (+ n 1) (cons n result)))
	  (else (loop (cdr lst) (+ n 1) result)))))

(define (count key lst)
  (let loop ((lst lst)(n 0))
	(cond
	  ((null? lst)n)
	  ((eq? (car lst) key)(loop (cdr lst) (+ n 1)))
	  (else (loop (cdr lst) n)))))

(define (game-end)
  (and (eq? turn 61)
	   (let ((black-count (count 'black (vector->list *board*)))
			 (white-count (count 'white (vector->list *board*))))
		 (cond 
		   ((< black-count white-count)
			(display "\nYou loss!!!!\n")(exit))
		   ((> black-count white-count)
			(display "\nYou win!!!!\n")(exit))
		   (else (display "\nDraw!!!!\n")(exit))))))

(define (print-board)
  (let ((lst '((black . "●") (white . "◯") (empty . "."))))
	(display " 1 2 3 4 5 6 7 8\n")
	(display 1)
	(let loop ((x 0)(n 2))
	  (format #t "~A " (cdr (assq (vector-ref *board* x) lst)))
	  (cond
		((= x 63)#t)
		((or (= x 7) (= x 15) (= x 23) (= x 31) (= x 39) (= x 47) (= x 55) (= x 63))
		 (begin
		   (newline)
		   (display n)
		   (loop (+ x 1)(+ n 1))))
		(else (loop (+ x 1) n))))))

;;数字の場合はそのまま、(縦 横)のリストで来たら座標を計算するようにした。
(define (input)
  (let loop ()
    (display "\n> ")(flush)
    (let ((pos (read)))
      (cond
       ((eq? pos 'pass)#t)
       (else
        (if (pair? pos)
            (set! pos (+
                       (* (- (car pos) 1) 8)
                       (- (cadr pos) 1))))
        (if (and (<= pos 63) (eq? (board-ref pos) 'empty))
            (board-set! pos 'black)
            (begin
              (display "Error")
              (loop)))
        (jadge? 'black pos))))))

(define (computer)
  (let* ((lst (filter-map (lambda(x)(computer-do x)) '(1 7 8 9)))
		 (pos (car lst)))
	(board-set! pos 'white)
	(jadge? 'white pos)))

(define (computer-do n)
  (let ((white (position 'white (vector->list *board*))))
	(let loop ((white white))
	  (cond
		((null? white)#f)
		((eq? (board-ref (+ (car white) n)) 'black)
		 (let loop2 ((x (+ (car white) n)))
		   (cond
			 ((eq? (board-ref (+ x n)) 'empty)
			  (+ x n))
			 ((eq? (board-ref (+ x n)) 'white)#f)
			 (else (loop2 (+ x n))))))
		((eq? (board-ref (- (car white) n)) 'black)
		 (let loop2 ((x (- (car white) n)))
		   (cond
			 ((eq? (board-ref (- x n)) 'empty)
			  (begin
				(- x n)))
			 ((eq? (board-ref (- x n)) 'white)#f)			 
			 (else
			   (loop2 (- x n))))))
		(else (loop (cdr white)))))))

(define (main args)
  (map (lambda(x)(board-set! x 'white)) '(27 36))
  (map (lambda(x)(board-set! x 'black)) '(28 35))
  (print-board)
  (let loop ()
	(begin
	  (input)
	  (print-board)
	  (newline)
	  (newline)
	  (inc! turn)
	  (game-end)
	  (computer)
	  (print-board)
	  (newline)
	  (inc! turn)
	  (game-end)
	  (loop))))