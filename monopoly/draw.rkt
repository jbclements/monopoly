#lang racket

(require "monopoly.rkt"
         2htdp/image
         rackunit)

(define BOARD-SIDE 500)
(define TRACK-WIDTH 50)
(define CELL-WIDTH (/ (- BOARD-SIDE (* 2 TRACK-WIDTH)) 9))

(define background 
  (rectangle BOARD-SIDE BOARD-SIDE "solid" "lightgray"))

(define (posn-dims posn)
  (cond
    [(= posn 0) (list (- BOARD-SIDE TRACK-WIDTH)
                      (- BOARD-SIDE TRACK-WIDTH)
                      TRACK-WIDTH
                      TRACK-WIDTH)]
    [(< posn 10) (list (+ TRACK-WIDTH (* (- 9 posn) CELL-WIDTH))
                       (- BOARD-SIDE TRACK-WIDTH)
                       CELL-WIDTH
                       TRACK-WIDTH)]
    [(= posn 10) (list 0
                       (- BOARD-SIDE TRACK-WIDTH)
                       TRACK-WIDTH
                       TRACK-WIDTH)]
    [(< posn 20) (list 0
                       (+ TRACK-WIDTH (* (- 19 posn) CELL-WIDTH))
                       TRACK-WIDTH
                       CELL-WIDTH)]
    [(= posn 20) (list 0
                       0
                       TRACK-WIDTH
                       TRACK-WIDTH)]
    [(< posn 30) (list (+ TRACK-WIDTH (* (- posn 21) CELL-WIDTH))
                       0
                       CELL-WIDTH
                       TRACK-WIDTH)]
    [(= posn 30) (list (- BOARD-SIDE TRACK-WIDTH)
                       0
                       TRACK-WIDTH
                       TRACK-WIDTH)]
    [(< posn 40) (list (- BOARD-SIDE TRACK-WIDTH)
                       (+ TRACK-WIDTH (* (- posn 31) CELL-WIDTH))
                       TRACK-WIDTH
                       CELL-WIDTH)]))

(check-equal? (posn-dims 0)
              (list (- BOARD-SIDE TRACK-WIDTH)
                    (- BOARD-SIDE TRACK-WIDTH)
                    TRACK-WIDTH
                    TRACK-WIDTH))

(define (draw-space-on posn background)
  (match-define (list x y dx dy) (posn-dims posn))
  (place-image/align (overlay 
                      (rectangle dx dy "outline" "black")
                      (rectangle dx dy "solid" "white"))
                     x y "left" "top"
                     background))

(for/fold ([img background]) ([posn (in-range 0 40)])
  (draw-space-on posn img))