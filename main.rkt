#lang racket
;(require "declarations.rkt")
;(require "150070001.rkt")
(require "drawing-routine.rkt")

(define iter 10000)    ;decides number of iterations to run
(define drawtime 100)    ;Draw every drawtime iterations
(define mouse-click-counter 0) 

(define (main)
  (define (main-helper i)
    (cond [(= done 0)
    (let*  ([ps-next 1])
                  (if (= (remainder iter drawtime) 0)
                      (begin 
                        (if (and (= debouncer 0) (= mouse-click-counter 0))
                            (set! mouse-click-counter 5)
                            (if (= debouncer 0)
                                (set! mouse-click-counter (- mouse-click-counter 1))
                                (display "")))
                        (cond ((= mouse-click-counter 0)
                            (update)))
                            
                            
                        ;(add-a-circle (floor (/ i 10))(floor (/ i 10)))
                        (draw-config)
                        (main-helper (+ i 1)))
                      (main-helper (+ i 1))))]))
  (main-helper 0))

(main)
(draw-config)
(show-final)

