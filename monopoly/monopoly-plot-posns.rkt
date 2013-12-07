#lang racket

(require plot)

;; plot distributions of monopoly landing patterns

(provide (contract-out [plot-posn-distr 
                        (-> (listof number?) any)]))

(define (plot-posn-distr distr)
  (define samples (length distr))
  (define the-hash
    (for/fold ([ht (hash)]) ([d distr])
      (hash-set ht d (add1 (hash-ref ht d 0)))))
  (define distr-list
    (hash-map
     the-hash
     (lambda (k v)
       (define frac (/ v samples))
       ;; 1% confidence interval:
       (define se (sqrt (/ (* frac (- 1 frac)) samples)))
       (define error (* 2.58 se))
       (vector k (ivl (- frac error) (+ frac error))))))
  (list
   (plot
    #:width 800
    (discrete-histogram
     distr-list))
   distr-list))