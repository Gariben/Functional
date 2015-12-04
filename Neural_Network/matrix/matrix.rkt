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


(define (rows-acc m rows)
  (cond
    [(empty? m) rows]
    [else
     (cond
       [(= (length (first m)) rows)
        (rows-acc (rest m) rows)]
       [else error]
       )]
    ))

;;Matrix integrity functions
;;Used to sanitize inputs and outputs
;;--------------------------------------------------------------------------------------------------

;;input-sanitize
;;convert lon to lol
;;check integrity of matrix
(define (input-sanitize in)
  (cond
    [(list? (first in)) (integrity? in)]
    [else (integrity? (lon2lol in))]
    ))

;;output-sanitize
;;convert lol to lon
;;check integrity of matrix
(define (output-sanitize out)
  (cond
    [(list? (first out))
     (cond
       [(= (length (first out)) 1)
        (lol2lon out)]
       [else (integrity? out)]
       )]
    [else error]
    ))

(define (lon2lol lon)
    (map (lambda (number)
         (list number))
       lon))

(define (lol2lon lol)
    (map (lambda (list)
         (first list))
       lol))

;;Integrity
;;makes sure that rows are uniform throughout matrix
(define (integrity? m)
  (integrity-check m (length (first m)) m))

(define (integrity-check m rows original)
  (cond
    [(empty? m) original]
    [else
     (cond
       [(= (length (first m)) rows)
        (integrity-check (rest m) rows original)]
       [else error]
       )]
    ))

(check-equal? (input-sanitize '(1 2 3 4)) '((1) (2) (3) (4)))
(check-equal? (output-sanitize '((1) (2) (3) (4))) '(1 2 3 4))
(check-equal? (output-sanitize (input-sanitize '(1 2 3 4))) '(1 2 3 4))