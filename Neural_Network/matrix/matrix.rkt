#lang racket
(require plai rackunit)

(define (error) empty)

;;columns
(define (columns m) (length m))
(check-equal? (columns '(1 2 3)) 3)

;;columns?
(define (columns? m)
  (if (empty? m) #f #t))
(check-equal? (columns? '(1 2 3)) #t)

;;rows
;;(define (rows m)
;;  (rows-acc m (length (first m))))

(define (rows m)
  (rows-acc m
            (cond
              [(list? m)
                (length (first m))]
              [else (first m)]
              )))


(define (rows-acc m cols)
  (cond
    [(empty? m) cols]
    [else
     (cond
       [(= (length (first m)) cols)
        (rows-acc (rest m) cols)]
       [else error]
       )]
    ))

