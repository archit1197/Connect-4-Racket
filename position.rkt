#lang racket

(provide Position%)

(define Position%
  (class object%
    (field (width 7)) ;corresponding to 7 columns
    (field (height (+ 6 1))) ;There are 6 rows in the board. An additional row is added at the top which stores the number of moves played in that column.
    (define vec-board (make-2d-vector height width 0)) ;the current state of the board stored as a 2d vector
    (define moves-played 0) ;number of moves played on the baord. Maximum of 42 moves can be played.

    (super-new)

    ;checks whether a column is completely filled or not
    (define/public (canPlay col)
      (> (- height 1) (2d-vector-ref vec-board 0 col)))

    ;helper function that displays the state of the board
    (define/public show
      (lambda()
        (define (print-helper i)
          (cond ((< i height)
                 (begin 
                   (display (vector-ref vec-board i))
                   (newline)
                   (print-helper (+ i 1))))))
        (print-helper 0)))

    ;return the current state of the board as a vector
    (define/public get-vector-board
      (lambda () vec-board))

    ;returns the number of moves alredy played on the board
    (define/public (nbMoves)
      moves-played)
    
    (define/public set-width
      (lambda (w) (set! width w)))

    (define/public set-height
      (lambda (h) (set! height h)))

    (define/public set-vector
      (lambda (v r c) (set! vec-board (copy-2d v r c))))

    (define/public set-moves
      (lambda (m) (set! moves-played m)))
    
    ;creating a copy of Board
    (define/public clone
      (lambda()
        (define temp (make-object Position%))
        (begin
          (send temp set-width width)
          (send temp set-height height)
          (send temp set-vector vec-board width height)
          (send temp set-moves moves-played)
          temp)))
      
    ;member function that modifies the board by placing a coin in the given column for the given player
    (define/public (play col player-num)
      (begin
        (set! moves-played (+ moves-played 1))
        (2d-vector-set! vec-board 0 col (+ 1 (2d-vector-ref vec-board 0 col)))
        (2d-vector-set! vec-board (- height (2d-vector-ref vec-board 0 col)) col player-num)))

    ;heuristic function returns a score based on the number of 3 coins for the given player
    ;that can be extended to form a winning combination(four in a row)
    (define/public heuristic
      (lambda (player)
        (define score1 (iterate player))
        (define score2 (iterate (- 3 player)))
        (- score1 score2)))


    (define (vertical x y player)
      (define h (- height (2d-vector-ref vec-board 0 y)))
      (cond [(and (<= h (- height 3))
                  (= (2d-vector-ref vec-board x y) 0)
                  ( = (2d-vector-ref vec-board (+ x 1) y) player)
                  ( = (2d-vector-ref vec-board (+ x 2) y) player)
                  ( = (2d-vector-ref vec-board (+ x 3) y) player)) #t]
            [#t #f]))

    ;iterates over all empty spaces to check for possible 3to4 patterns      
    (define/public (iterate player)
      (define count 0)
      (define x 0)
      (define y 0)
      (begin
        (for (set! x 1) : (< x 7) : (set! x (+ x 1)) :
          (for (set! y 0) : (< y 7) : (set! y (+ y 1)) :
            (cond [(= (2d-vector-ref vec-board x y) 0)
                   (begin
                     (define b1 (vertical x y player))
                     (define b2 (abstract_four x y player 0 1))
                     (define b3 (abstract_four x y player 1 1))
                     (define b4 (abstract_four x y player 1 -1))
                     (if (or b1 b2 b3 b4) (set! count (+ count 1))
                         (set! count count)))])))
        count))
    
    ;abstraction for checking the possible 3to4 patterns in all directions
    ; value of a and b determines the direction to be checked
    (define (abstract_four x y player a b)
      (cond
        ;pattern where empty square has 1 to its left and 2 to its right
        [(and (= (2d-vector-ref vec-board x y) 0)
              (and (>= y b) (> x a) (< (- x a) width) (< (- y b) width) (= (2d-vector-ref vec-board (- x a) (- y b)) player))
              (and (< y (- width b)) (< x (- width a)) (>  (+ x a) 0) (>= (+ y b) 0)  (= (2d-vector-ref vec-board (+ x a) (+ y b)) player))
              (and (< y (- width (* 2 b))) (> (+ x (* 2 a)) 0) (>= (+ y (* 2 b)) 0) (< x (- width (* 2 a))) (= (2d-vector-ref vec-board (+ x (* 2 a)) (+ y (* 2 b))) player))) #t]
        ;pattern where empty square has 2 to its left and 1 to its right      
        [(and (= (2d-vector-ref vec-board x y) 0)
              (and (>= y b) (> x a) (< (- x a) width) (< (- y b) width) (= (2d-vector-ref vec-board (- x a) (- y b)) player))
              (and (< y (- width b)) (< x (- width a)) (> (+ x a) 0) (>= (+ y b) 0) (= (2d-vector-ref vec-board (+ x a) (+ y b)) player))
              (and (>= y (* 2 b)) (> x (* 2 a)) (< (- x (* 2 a)) width) (< (- y (* 2 b)) width) (= (2d-vector-ref vec-board (- x (* 2 a)) (- y (* 2 b))) player))) #t]
        ;pattern where empty square has 3 to its left       
        [(and (= (2d-vector-ref vec-board x y) 0)
              (and (>= y b) (> x a) (< (- x a) width) (< (- y b) width)  (= (2d-vector-ref vec-board (- x a) (- y b)) player))
              (and (>= y (* 2 b)) (> x (* 2 a)) (< (- x (* 2 a)) width) (< (- y (* 2 b)) width)  (= (2d-vector-ref vec-board (- x (* 2 a)) (- y (* 2 b))) player))
              (and (>= y (* 3 b)) (> x (* 3 a)) (< (- x (* 3 a)) width) (< (- y (* 3 b)) width)  (= (2d-vector-ref vec-board (- x (* 3 a)) (- y (* 3 b))) player))) #t]               
        ;pattern where empty square has 3 to its right 
        [(and (= (2d-vector-ref vec-board x y) 0)
              (and (< y (- width b)) (< x (- width a)) (> x a) (>= (+ y b) 0) (= (2d-vector-ref vec-board (+ x a) (+ y b)) player))
              (and (< y (- width (* 2 b))) (< x (- width (* 2 a))) (> (+ x (* 2 a)) 0) (>= (+ y (* 2 b)) 0) (= (2d-vector-ref vec-board (+ x (* 2 a)) (+ y (* 2 b))) player))
              (and (< y (- width (* 3 b))) (< x (- width (* 3 a))) (> (+ x (* 3 a)) 0) (>= (+ y (* 3 b)) 0) (= (2d-vector-ref vec-board (+ x (* 3 a)) (+ y (* 3 b))) player))) #t]
        [#t #f]))                
    

    ;to check if playing a move in a given column results in a win for the 'player'
    ;uses the abstraction defined earlier
    (define/public (isWinningMove col player)
      (define h (- height (2d-vector-ref vec-board 0 col)))
      (define x (- h 1))
      (define y col)
      ;first checks if canPlay in the given column
      ;if true, checks if there are four consecutive circles of 'player' in any of vertical, horizontal, top left-bottom right or top right-bottom left direction
      (cond [(canPlay col) (begin                             
                             (define b1 (vertical x y player))
                             (define b2 (abstract_four x y player 0 1))
                             (define b3 (abstract_four x y player 1 1))
                             (define b4 (abstract_four x y player 1 -1))
                             (if (or b1 b2 b3 b4) #t
                                 #f))]
            [#t #f]))     
    ))


;helper functions for 2d vectors
(define (make-2d-vector r c init)
  (build-vector r (lambda (x) (make-vector c init))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let [(v (vector-ref vec r))]
    (begin
      (vector-set! v c val)
      (vector-set! vec r v))))

;returns a copy of the 2d-vector
(define (copy-2d v1 r c)
  (define v (make-vector r 0))
  (define (copy-helper i)
    (cond [(< i (- r 1)) (begin
                           (vector-set! v i (vector-copy (vector-ref v1 i))))
                         (copy-helper (+ i 1))]
          [(= i (- r 1)) (vector-set! v i (vector-copy (vector-ref v1 i)))]))
  (begin
    (copy-helper 0)
    v))

  
(define (subvector v start end)
  (build-vector (- end start) (lambda (i) (vector-ref v (+ i start)))))

;for loop macro
(define-syntax for
  (syntax-rules (:)
    [(for init : condition : step : statements ...)
     (begin
       init
       (define (iter)
         (cond [condition (begin statements ... step (iter))]))
       (iter))]))