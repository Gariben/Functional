#lang racket

(struct wins [stay switch] #:transparent)

(define (random-door) 
  (+ 1 (random 3)))

(define (one-in-three choice)
  (cond
    [(= choice 1) '(1 0 0)]
    [(= choice 2) '(0 1 0)]
    [(= choice 3) '(0 0 1)]
    [else (error "ERROR: Incorrect choice given for scenario!")]
    ))

(define (gen3) (one-in-three 3))

(define (trials size winc counter)
  (cond
    [(= counter size) winc]
    [(false? (= 1 (list-ref (gen3) (- (random-door) 1)))) 
     (trials size (wins (wins-stay winc) (+ 1(wins-switch winc))) (+ 1 counter))]
    [else (trials size (wins (+ 1 (wins-stay winc)) (wins-switch winc)) (+ 1 counter))]
    ))

(define (Monty-Hall size)
  (trials size (wins 0 0) 0))

;;==================================================================================================

(define total-test (Monty-Hall 1000))