#lang racket

(require racket/trace)
(require "position.rkt")

(provide b1 S)

;board is a 2d-vector, each cell has 3 values;
;0 means empty, 1 means player, 2 means computer;

(define Solver%
  (class object%

    (define nodeCount 0)
    (define column_exploration_order (list 3 4 2 5 1 0 6))
    
    (super-new)

    ;function implements minmax algorithm
    ;It searches upto a fixed depth and returns a score determined by the heuristic function defined in
    ;Position class
    (define/public minmax
      (lambda (P alpha beta player depth)
        (begin
          (set! nodeCount (+ 1 nodeCount))
          (define flag1 1)
          (define max -22)
          (define result 0)
          (define x 0)
          (define s0 1)
          (define s1 1)
          (define s2 1)
          (define n-winning-1 0)
          (define n-winning-2 0)
          ;if board is in draw state no position to move to;
          (cond [(= (send P nbMoves) 42)
                 (begin
                   (set! flag1 0)
                   (set! result (cons 0 -1)))])
          (cond [(= (send P nbMoves) 0)
                 (begin
                   (set! flag1 0)
                   (set! result (cons 0 3)))])
          (cond [(= (send P nbMoves) 1)
                 (begin
                   (set! flag1 0)
                   (set! result (cons 0 3)))])
          (cond [(= (send P nbMoves) 2)
                 (begin
                   (set! flag1 0)
                   (define v (send P get-vector-board))
                   (cond [(= (2d-vector-ref v 0 3) 1) (set! result (cons 0 3))]
                         [(= (2d-vector-ref v 0 3) 2) (set! result (cons 0 4))]))])
          (cond [(= (send P nbMoves) 3)
                 (begin
                   (set! flag1 0)
                   (define v (send P get-vector-board))
                   (cond [(and (= (2d-vector-ref v 0 3) 2) (= (2d-vector-ref v 0 2) 0)) (set! result (cons 0 2))]
                         [(and (= (2d-vector-ref v 0 3) 2) (= (2d-vector-ref v 0 4) 0)) (set! result (cons 0 4))]
                         [(= (2d-vector-ref (send P get-vector-board) 0 3) 1) (set! result (cons 0 3))]))])
          (cond [(= depth 0)
                 (begin
                   (set! flag1 0)
                   (set! result (cons (send P heuristic player) -1)))])
          (cond [(= flag1 0) result]
                [#t (begin
                      (set! n-winning-1 0)
                      (set! n-winning-2 0)
                      (for (set! x 0) : (< x 7) : (set! x (+ x 1)) :
                        (define val (list-ref column_exploration_order x))
                        (cond [(and (send P canPlay val) (send P isWinningMove val player) (= player 1))
                               (begin 
                                 (set! s0 0)
                                 (set! result (cons 100 val))
                                 (set! n-winning-1 (+ 1 n-winning-1)))])
                        (cond [(and (send P canPlay val) (send P isWinningMove val player) (= player 2))
                               (begin 
                                 (set! s0 0)
                                 (set! result (cons -200 val))
                                 (set! n-winning-2 (+ 1 n-winning-2)))])
                        )
                      (if (> n-winning-1 0) (set! result (cons (* n-winning-1 100) (cdr result)))
                          (if (> n-winning-2 0) (set! result (cons (* n-winning-2 -200) (cdr result)))
                              (display "")))
                      (if (= s0 0) result                                                                        
                          (begin
                            ;not terminal state
                            (cond [(= player 1)
                                   (begin
                                     (define bestval -1000)
                                     (define fcol 0)
                                     (for (set! x 0) : (< x 7) : (set! x (+ x 1)) :
                                       (define val (list-ref column_exploration_order x))
                                       (cond [(send P canPlay val)
                                              (begin                                                
                                                (define pos (send P clone))
                                                (send pos play val player)
                                                (define pr (minmax pos alpha beta 2 (- depth 1)))
                                                (define score (car pr))                                                 
                                                (cond [(>= score bestval) (begin                                                                         
                                                                            (set! bestval score)
                                                                            (set! fcol val))]
                                                      [(> bestval alpha) (begin
                                                                           (set! alpha bestval))]
                                                      [(<= beta alpha) (begin
                                                                         (set! x 10))]))]))
                                     (set! result (cons bestval fcol)))])

                            (cond [(= player 2)
                                   (begin
                                     (define bestval 1000)
                                     (define fcol 0)
                                     (for (set! x 0) : (< x 7) : (set! x (+ x 1)) :
                                       (define val (list-ref column_exploration_order x))
                                       (cond [(send P canPlay val)
                                              (begin                                                
                                                (define pos (send P clone))
                                                (send pos play val player)
                                                (define pr (minmax pos alpha beta 1 (- depth 1)))
                                                (define score (car pr))                                                 
                                                (cond [(<= score bestval) (begin
                                                                            (set! bestval score)
                                                                            (set! fcol val))]
                                                      [(< bestval beta) (begin
                                                                          (set! beta bestval))]
                                                      [(<= beta alpha) (begin
                                                                         (set! x 10))]))]))
                                     (set! result (cons bestval fcol)))])                            
                            result)))]))))

    (define/public (solve P)
      (begin
        (set! nodeCount 0)
        (minmax P -1000 1000 1 6)))

    (define/public getNodeCount
      (lambda ()
        nodeCount))))        


(define-syntax for
  (syntax-rules (:)
    [(for init : condition : step : statements ...)
     (begin
       init
       (define (iter)
         (cond [condition (begin statements ... step (iter))]))
       (iter))]))

(define b1 (make-object Position%))
(define S (make-object Solver%))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))
