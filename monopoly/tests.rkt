#lang racket

(require "monopoly.rkt"
         rackunit)

;;
;; TEST CASES
;;



(check-equal? (move-player (player 38 1500 #f) 2 #t)
              (player 2 (+ 1500 GO-BONUS) #f))
(check-equal? (move-player (player 24 1500 #f) 31 #t)
              (player 31 1500 #f))


(check-equal? (remove-player (id 0) init-state)
              (gamestate1 (vector (id 1) (id 2) (id 3))
                         2
                         init-pmap
                         (hash)))
(check-equal? (remove-player (id 2) init-state)
              (gamestate1 (vector (id 0) (id 1) (id 3))
                         0
                         init-pmap
                         (hash)))

(check-equal? (transfer-properties-from-player (id 1)
                                               (id 3)
                                               (gamestate1
                                                (vector (id 0) (id 1) (id 2) (id 3))
                                                1
                                                (hash (id 0) (vector 0 1500)
                                                      (id 1) (vector 0 1500)
                                                      (id 2) (vector 0 1500)
                                                      (id 3) (vector 0 1500))
                                                (hash 5 (id 1)
                                                      15 (id 0)
                                                      24 (id 3)
                                                      25 (id 1))))
              (gamestate1
               (vector (id 0) (id 1) (id 2) (id 3))
               1
               (hash (id 0) (vector 0 1500)
                     (id 1) (vector 0 1500)
                     (id 2) (vector 0 1500)
                     (id 3) (vector 0 1500))
               (hash 5 (id 3)
                     15 (id 0)
                     24 (id 3)
                     25 (id 3))))

;; TRANSFER-TO-BANK
(check-equal? (maybe-transfer-to-bank 100 (id 3) init-state)
              (gamestate1 (vector (id 0) (id 1) (id 2) (id 3))
                         0
                         (hash (id 0) (player 0 1500 #f)
                               (id 1) (player 0 1500 #f)
                               (id 2) (player 0 1500 #f)
                               (id 3) (player 0 1400 #f))
                         (hash)))

;; NEXT-TURN
(check-equal? (next-turn init-state)
              (gamestate1
               (vector (id 0) (id 1) (id 2) (id 3))
               1
               init-pmap
               (hash)))
(check-equal? (next-turn (gamestate1
                          (vector (id 0) (id 1) (id 3) (id 2))
                          3 
                          init-pmap
                          (hash)))
              (gamestate1
               (vector (id 0) (id 1) (id 3) (id 2))
               0
               init-pmap
               (hash)))

(check-equal? (transfer-money 234 (id 1) (id 2)
                              init-state)
              (gamestate1
               (vector (id 0) (id 1) (id 2) (id 3))
               0
               (hash (id 0) (player 0 1500 #f)
                     (id 1) (player 0 (- 1500 234) #f)
                     (id 2) (player 0 (+ 1500 234) #f)
                     (id 3) (player 0 1500 #f))
               (hash)))

(check-equal? (maybe-transfer-money 234 (id 1) (id 2)
                                    init-state)
              (gamestate1
               (vector (id 0) (id 1) (id 2) (id 3))
               0
               (hash (id 0) (player 0 1500 #f)
                     (id 1) (player 0 (- 1500 234) #f)
                     (id 2) (player 0 (+ 1500 234) #f)
                     (id 3) (player 0 1500 #f))
               (hash)))

;; RENT-OWED 

(define test-travel-info (travel-info 3 #f))

(check-equal? (rent-owed 27 (travel-info 4 #f)
                         (gamestate (vector (id 3) (id 4))
                                     0
                                     (vector (player 27 1234 #f)
                                             (player 1 1500 #f))
                                     (hash 27 (property-state (id 4) 2)
                                           26 (property-state (id 4) 2) 
                                           29 (property-state (id 4) 2))
                                     (list empty empty)))
              330)

(check-equal? (rrs-owned-by (id 3) 
                            (make-prmap
                             (hash 5 (id 1) 15 (id 3) 34 (id 0) 35 (id 3))))
              2)

(check-equal? (rent-owed 28 (travel-info 9 #f)
                         (gamestate1 (vector (id 3) (id 4))
                                    0
                                    (hash)
                                    (hash 28 (id 4))))
              (* 9 4))

(check-equal? (rent-owed 15
                         test-travel-info
                         (gamestate1 (vector (id 3))
                                    0
                                    (hash)
                                    (hash 15 (id 3))))
              25)
(check-equal? (rent-owed 15 test-travel-info
                         (gamestate1 (vector (id 3))
                                     0
                                     (hash)
                                     (hash 15 (id 3) 25 (id 3))))
              50)
(check-equal? (rent-owed 15
                         (travel-info 3 'go-to-rr)
                         (gamestate1 (vector (id 3))
                                     0
                                     (hash)
                                     (hash 15 (id 3) 25 (id 3))))
              100)
(check-equal? (rent-owed 15 test-travel-info
                         (gamestate1 (vector (id 3))
                                     0
                                     (hash)
                                     (hash 15 (id 3) 5 (id 3) 25 (id 3))))
              100)
(check-equal? (rent-owed 15 test-travel-info
                         (gamestate1 (vector (id 3))
                                       0
                                       (hash)
                                       (hash 15 (id 3) 25 (id 3) 35 (id 3) 5 (id 3))))
              200)

(check-equal? (rent-owed 24 test-travel-info
                         (gamestate1 (vector (id 3))
                                     0 
                                     (hash)
                                     (hash 24 (id 3))))
              20)

;; BUY-PROPERTY


(check-equal? (maybe-buy-property (gamestate1 init-tvec
                                             3
                                             (hash (id 0) (player 0 1500 #f)
                                                   (id 1) (player 0 1500 #f)
                                                   (id 2) (player 0 1500 #f)
                                                   (id 3) (player 24 1500 #f))
                                             (hash)))
              (gamestate1 init-tvec
                         3
                         (hash (id 0) (player 0 1500 #f)
                               (id 1) (player 0 1500 #f)
                               (id 2) (player 0 1500 #f)
                               (id 3) (player 24 (- 1500 240) #f))
                         (hash 24 (id 3))))

;; can't afford it:
(check-equal? (maybe-buy-property (gamestate1 init-tvec
                                             3
                                             (hash (id 0) (player 0 1500 #f)
                                                   (id 1) (player 0 1500 #f)
                                                   (id 2) (player 0 1500 #f)
                                                   (id 3) (player 24 100 #f))
                                             (hash)))
              (gamestate1 init-tvec
                         3
                         (hash (id 0) (player 0 1500 #f)
                               (id 1) (player 0 1500 #f)
                               (id 2) (player 0 1500 #f)
                               (id 3) (player 24 100 #f))
                         (hash)))



;; player 3 buys property successfully
(check-equal? (buy-property (gamestate1 init-tvec
                                       3
                                       (hash (id 0) (player 0 1500 #f)
                                             (id 1) (player 0 1500 #f)
                                             (id 2) (player 0 1500 #f)
                                             (id 3) (player 24 1500 #f))
                                       (hash)))
              (gamestate1 init-tvec
                         3
                         (hash (id 0) (player 0 1500 #f)
                               (id 1) (player 0 1500 #f)
                               (id 2) (player 0 1500 #f)
                               (id 3) (player 24 (- 1500 240) #f))
                         (hash 24 (id 3))))

(check-equal? (buy-property (gamestate1 init-tvec
                                       3
                                       (hash (id 0) (player 0 1500 #f)
                                             (id 1) (player 0 1500 #f)
                                             (id 2) (player 0 1500 #f)
                                             (id 3) (player 5 1500 #f))
                                       (hash)))
              (gamestate1 init-tvec
                         3
                         (hash (id 0) (player 0 1500 #f)
                               (id 1) (player 0 1500 #f)
                               (id 2) (player 0 1500 #f)
                               (id 3) (player 5 (- 1500 200) #f))
                         (hash 5 (id 3))))

(check-equal? (make-move
               (gamestate1 (vector (id 0))
                          0
                          (hash (id 0) (player 38 1500 #f))
                          (hash))
               6)
              (gamestate1 (vector (id 0))
                          0
                          (hash (id 0) (player 4 (+ 1500 GO-BONUS) #f))
                          (hash)))

;; player 9 lands on luxury tax, pays $75
(check-equal? (make-move 
               (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 32 128 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)))
               6)
              (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 38 (- 128 75) #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9))))
;; player 9 lands on luxury tax, goes bankrupt
(check-equal? (make-move 
               (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 32 28 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)))
               6)
              (gamestate1 (vector (id 1) (id 25))
                          0
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 38 0 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1))))

;; player 9 goes bankrupt, gives everything to player 1:
(check-equal? (make-move 
               (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 19 28 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)))
               6)
              (gamestate1 (vector (id 1) (id 25))
                         0
                         (hash (id 1) (player 12 (+ 1234 28) #f)
                               (id 2) (player 23 0 #f)
                               (id 9) (player 25 0 #f)
                               (id 25) (player 0 4423 #f))
                         (hash 25 (id 1) 5 (id 1) 15 (id 1))))

;; player 9 pays monopoly price to player 1
(check-equal? (make-move
               (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 19 428 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)))
               7)
              (gamestate1 (vector (id 1) (id 9) (id 25))
                         1
                         (hash (id 1) (player 12 (+ 1234 44) #f)
                               (id 2) (player 23 0 #f)
                               (id 9) (player 26 (- 428 44) #f)
                               (id 25) (player 0 4423 #f))
                         (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1))))

;; player 9 lands in jail
(check-equal? (make-move
               (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 19 428 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)))
               11)
              (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 10 428 #t)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1))))

;; player 9 is in jail, pays $50 then moves
(check-equal? (take-turn
               (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 10 428 #t)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)))
               (lambda () '(6 4)))
              (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 20 (- 428 50) #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1))))

;; player 9 rolls double 4s and then a 6&4
(check-equal? (take-turn
               (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 10 428 #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)))
               (call-with-values (lambda () (sequence-generate '((5 5) (2 3))))
                                 (lambda (a b) b)))
              (gamestate1 (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 (+ 1234 50) #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 25 (- 428 50) #f)
                                (id 25) (player 0 4423 #f))
                          (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1))))


;; CHANCE CARDS

;; player 9 draws the "take a ride on the reading" chance card, moves
;; there and collects $200, pays owner $50:
(check-equal? (handle-landing
               (travel-info 3 #f)
               (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 36 428 #f)
                                (id 25) (player 0 4423 #f))
                          (make-prmap
                           (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)))
                          (list (list
                                 'go-to-reading
                                 'go-to-illinois)
                                (list))))
              (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 (+ 1234 50) #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 5 (+ 200 (- 428 50)) #f)
                                (id 25) (player 0 4423 #f))
                          (make-prmap
                           (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                 26 (id 1) 27 (id 1) 29 (id 1)))
                          (list (list 
                                 'go-to-illinois
                                 'go-to-reading)
                                (list))))

;; player 9 lands on the water works, buys it
(check-equal? (handle-landing
               (travel-info 3 #f)
               (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 28 428 #f)
                                (id 25) (player 0 4423 #f))
                          (make-prmap
                           (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)))
                          (list empty empty)))
              (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 28 (- 428 150) #f)
                                (id 25) (player 0 4423 #f))
                          (make-prmap
                           (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                 26 (id 1) 27 (id 1) 29 (id 1)
                                 28 (id 9)))
                          (list empty empty)))

;; player 9 lands on the water works, pays player 1 4x
(check-equal? (handle-landing
               (travel-info 3 #f)
               (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 28 428 #f)
                                (id 25) (player 0 4423 #f))
                          (make-prmap
                           (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                 26 (id 1) 27 (id 1) 29 (id 1)
                                 28 (id 1)))
                          (list empty empty)))
              (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 (+ 1234 12) #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 28 (- 428 12) #f)
                                (id 25) (player 0 4423 #f))
                          (make-prmap
                           (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)
                                28 (id 1)))
                          (list empty empty)))

;; player 9 draws go-to-utility, pays player 1 10x
(check-equal? (handle-landing
               (travel-info 3 #f)
               (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 22 428 #f)
                                (id 25) (player 0 4423 #f))
                          (make-prmap
                           (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                 26 (id 1) 27 (id 1) 29 (id 1)
                                 28 (id 1)))
                          (list (list 'go-to-utility 'bogus) empty)))
              (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 (+ 1234 30) #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 28 (- 428 30) #f)
                                (id 25) (player 0 4423 #f))
                          (make-prmap
                           (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                 26 (id 1) 27 (id 1) 29 (id 1)
                                 28 (id 1)))
                          (list (list 'bogus 'go-to-utility) empty)))

;; player 9 draws go-to-rr, pays player 1 2x
(check-equal? (handle-landing
               (travel-info 3 #f)
               (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 22 428 #f)
                                (id 25) (player 0 4423 #f))
                          (make-prmap
                           (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                 26 (id 1) 27 (id 1) 29 (id 1)
                                 28 (id 1)))
                          (list (list 'go-to-rr 'bogus) empty)))
              (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 (+ 1234 100) #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 25 (- 428 100) #f)
                                (id 25) (player 0 4423 #f))
                          (make-prmap
                           (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                 26 (id 1) 27 (id 1) 29 (id 1)
                                 28 (id 1)))
                          (list (list 'bogus 'go-to-rr) empty)))

;; transfer-from-bank
(check-equal? (transfer-from-bank 
               50 
               (id 9)
               (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 22 428 #f)
                                (id 25) (player 0 4423 #f))
                          (make-prmap
                           (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                 26 (id 1) 27 (id 1) 29 (id 1)
                                 28 (id 1)))
                          (list (list 'go-to-rr 'bogus) empty)))
              (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 22 (+ 50 428) #f)
                                (id 25) (player 0 4423 #f))
                          (make-prmap
                           (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                26 (id 1) 27 (id 1) 29 (id 1)
                                28 (id 1)))
                          (list (list 'go-to-rr 'bogus) empty)))
;; 
;; player 9 draws move-back-3, lands on new york, buys it
(check-equal? (handle-landing
               (travel-info 3 #f)
               (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 22 428 #f)
                                (id 25) (player 0 4423 #f))
                          (make-prmap
                           (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                 26 (id 1) 27 (id 1) 29 (id 1)
                                 28 (id 1)))
                          (list (list 'go-back-3 'bogus) empty)))
              (gamestate (vector (id 1) (id 9) (id 25))
                          1
                          (hash (id 1) (player 12 1234 #f)
                                (id 2) (player 23 0 #f)
                                (id 9) (player 19 (- 428 200) #f)
                                (id 25) (player 0 4423 #f))
                          (make-prmap
                           (hash 25 (id 1) 5 (id 1) 15 (id 9)
                                 26 (id 1) 27 (id 1) 29 (id 1)
                                 28 (id 1)
                                 19 (id 9)))
                          (list (list 'bogus 'go-back-3) empty))
              )




(check-equal? (has-monopoly-on-color (id 4)
                                     'yellow
                                     (make-prmap
                                      (hash 29 (id 4) 27 (id 4)
                                            26 (id 4))))
              true)
(check-equal? (has-monopoly-on-color (id 4)
                                     'yellow
                                     (make-prmap
                                      (hash 29 (id 4) 27 (id 3)
                                            26 (id 4))))
              false)

(check-equal? (buy-house (id 0) 31
                         (gamestate (vector (id 0)) 1
                                    (hash (id 0) (player 13 1234 #f))
                                    (hash 31 (property-state (id 0) 1))
                                    (list empty empty)))
              (gamestate (vector (id 0)) 1
                         (hash (id 0) (player 13 (- 1234 200) #f))
                         (hash 31 (property-state (id 0) 2))
                         (list empty empty)))