#lang racket
;(require "declarations.rkt")
(require "connect4.rkt")
(require "position.rkt")
(require racket/gui)

(provide draw-config)
(provide add-a-circle)
(provide ai-play)
(provide done debouncer update show-final)

(define frame-size 700)
(define bitmap-size 700)
(define user-turn 1)
(define done 0)
(define debouncer 1)
(define final-string "GAME OVER!! \n")

;;;This is a function that helps in taking care of multiple unintentional clicks
(define (update)
  (set! debouncer 1))

  ; Make a 600 x 700 frame
(define frame (new frame% [label "CONNECT-4 SIMULATION"]
                   [width frame-size]
                   [height 630]))
(define my-canvas% 
  (class canvas% 
    (super-new)
    (define/override (on-event event)
      ;If mouse click event happens
      (cond [(send event get-left-down)
             (cond [(= debouncer 1)
          (let* ([col (floor (/ (send event get-x) 100))])
          (begin 
          (set! debouncer 0)
          (if (send b1 canPlay col)
              (begin
                (send msg set-label "Now wait for computer!")
                (add-a-circle (vector-ref last-height col) col 'red)
                (if (send b1 isWinningMove col 2)
                    (begin
                      (send b1 play col 2)
                      (vector-set! last-height col (- (vector-ref last-height col) 1))
                      (draw-config)
                      (display "Congratulations! You have won.")
                      (send msg set-label "Congratulations! You have won.")
                      (set! final-string (string-append final-string "YOU WON!!"))
                      (set! done 1))
                (begin    
                (send b1 play col 2)
                (vector-set! last-height col (- (vector-ref last-height col) 1))
                (draw-config)
                
                (set! user-turn 0)
                (ai-play))))
              (begin
                (send msg set-label "This column is filled!")))
          
          ))])]))))

(define msg (new message% [parent frame]
                            [label "START game by clicking in any column."]
                            [min-width 700]
                            [min-height 30]))
    
; Make the drawing area with a paint callback
(define canvas
  (new my-canvas% [parent frame]
       [min-width 700]
       [min-height 600]
       [paint-callback
        (lambda (canvas dc) (paint dc))]))

; ... pens, brushes, and draw-face are the same as above ...

(define (paint dc) (send dc draw-bitmap face-bitmap 0 0))

; ... pens, brushes, and draw-face are the same as above ...

; Create a bitmap
(define face-bitmap (make-object bitmap% bitmap-size bitmap-size ))
; Create a drawing context for the bitmap
(define bm-dc (make-object bitmap-dc% face-bitmap))
; A bitmap's initial content is undefined; clear it before drawing
(send bm-dc clear)

; Make some pens and brushes
(define black-pen (make-object pen% "BLACK" 1 'solid))
(define yellow-brush (make-object brush% "YELLOW" 'solid))
(define red-pen (make-object pen% "RED" 2 'solid))
(define no-brush (make-object brush% "BLACK" 'transparent))
(define brown-pen (make-object pen% "BROWN" 1 'solid))
(define red-brush (make-object brush% "RED" 'solid))


(define (draw-lines)
  (begin
    ;(send bm-dc clear)
    (send bm-dc set-brush no-brush)
    (send bm-dc set-pen brown-pen)
    ))

;;;These lists store the positions of the red and yellow coins
(define red-c '())
(define yellow-c '())

;;;Last position playable on each column
(define last-height (vector 5 5 5 5 5 5 5))

(define make-grid (new dc-path%))
(define (make-grid-helper n-vertical n-horizontal)
  (begin 
    (make-vertical n-vertical)
    (make-horizontal n-horizontal)
    (send make-grid close)
    ))
;Makes n equiwidth vertical lines 
(define (make-vertical n)
  (define (helper i)
    (send make-grid move-to (* i (/ frame-size n)) 0)
    (send make-grid line-to (* i (/ frame-size n)) frame-size)
    (cond ((not (= i n)) (helper (+ i 1)))))
  (helper 1))

;Makes n equiwidth horizontal lines
(define (make-horizontal n)
  (define (helper i)
    (send make-grid move-to 0 (* i (/ frame-size n)))
    (send make-grid line-to frame-size (* i (/ frame-size n)))
    (cond ((not (= i n)) (helper (+ i 1)))))
  (helper 1))

;Adds a circle in the list red-c, i.e the second players list
(define (add-a-circle i j colour)
  (cond ((eq? colour 'red)
         (begin
           (set! red-c (append red-c (list (cons i j))))
           ;(display red-c)
           ))
        ((eq? colour 'yellow)
         (begin
           (set! yellow-c (append yellow-c (list (cons i j))))
           ;(display yellow-c)
           ))
        ))

;;;This function computes the winning move for computer and plays yellow coin
(define (ai-play)
  (cond ((= user-turn 0)
         (let* ([winning-col (cdr (send S solve b1))])
                (cond ((= winning-col -1)
                       (begin 
                       (display "GaME IS drawed!!!!")
                       (send msg set-label "GaME IS drawed!!!!")
                       (set! final-string (string-append final-string "MATCH TIED!!"))
                       (set! done 1)))
                      (#t 
                       (let* ([winning-row (vector-ref last-height winning-col)])
                       (begin
                         (add-a-circle winning-row winning-col 'yellow)
                         (if (send b1 isWinningMove winning-col 1)
                             (begin 
                             (display "Better luck next time!")
                             (send msg set-label "GAME OVER...Better luck next time!")
                             (set! final-string (string-append final-string "COMPUTER WON!!"))
                             (send b1 play winning-col 1)
                             (set! done 1))
                         (begin
                         (send b1 play winning-col 1)
                         (vector-set! last-height winning-col (- (vector-ref last-height winning-col) 1))
                         
                         (set! user-turn 1)
                         (display "Computer has played!")
                         (send msg set-label "Now it's your turn!")))))))))))

; Show the frame
(send frame show #t)
;;;Draws the current configuration of the board
(define (draw-config)
  (begin
    (send bm-dc clear)
    (draw-lines)
    (make-grid-helper 7 7)
    (send bm-dc draw-path make-grid)
    ;;;Draw the red coins
    (send bm-dc set-brush red-brush)
    (send bm-dc set-pen red-pen)
    (map (lambda (p) (send bm-dc draw-ellipse (* (cdr p) (/ frame-size 7)) (* (car p) (/ frame-size 7)) 100 100)) red-c)
    ;(send bm-dc draw-text "GAME OVER!" 10 10)
    ;;;Draw the yellow coins
    (send bm-dc set-brush yellow-brush)
    (send bm-dc set-pen black-pen)
    (map (lambda (p) (send bm-dc draw-ellipse (* (cdr p) (/ frame-size 7)) (* (car p) (/ frame-size 7)) 100 100)) yellow-c)

   ; (draw-circles red-c)
    (send canvas refresh)
    (sleep/yield 0.01)))


  ; Make a 600 x 700 frame
(define frame2 (new frame% [label "CONNECT-4 SIMULATION"]
                   [width 300]
                   [height 300]))

; Make the drawing area with a paint callback
(define canvas2
  (new canvas% [parent frame2]
       [min-width 300]
       [min-height 300]
       [paint-callback
        (lambda (canvas2 dc) (paint2 dc))]))

; ... pens, brushes, and draw-face are the same as above ...

(define (paint2 dc) (send dc draw-bitmap face-bitmap2 0 0))

; ... pens, brushes, and draw-face are the same as above ...

; Create a bitmap
(define face-bitmap2 (make-object bitmap% bitmap-size bitmap-size ))
; Create a drawing context for the bitmap
(define bm-dc2 (make-object bitmap-dc% face-bitmap2))
; A bitmap's initial content is undefined; clear it before drawing
(send bm-dc2 clear)


(define (show-final)
  (begin
    (send frame2 show #t)
    (send bm-dc2 clear)
    (send bm-dc2 draw-text final-string 10 100)
    (send bm-dc2 set-brush red-brush)
    (send bm-dc2 set-pen red-pen)
    (send canvas2 refresh)
    (sleep/yield 0.01)))
    