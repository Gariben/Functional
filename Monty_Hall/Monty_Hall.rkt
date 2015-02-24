#lang racket

(require rackunit)


;;Part 1: Defining Monty, a selection of three doors
;;--------------------------------------------------------------------------------------------------

(struct Monty [door1 door2 door3] #:transparent)

;; -> Monty
(define (init-Monty) (Monty -1 -1 -1))

;;Integer->Monty
(define (set-Monty choice)
  (cond
    [(= choice 1) (Monty 1 0 0)]
    [(= choice 2) (Monty 0 1 0)]
    [(= choice 3) (Monty 0 0 1)]
    [else (error "ERROR: Incorrect choice given for scenario!")]
    ))

;; ->Integer
(define (random-door) (+ 1 (random 3)))

;; ->Monty
(define (random-Monty) 
  (set-Monty (random-door)))

;; Monty->list
(define (Monty-to-list uMonty) (list (Monty-door1 uMonty) (Monty-door2 uMonty) (Monty-door3 uMonty)))

;; Monty-list, Monty-> Monty
(define (list-to-Monty-handler M-list uMonty) 
  (cond
    [(= (length M-list) 3) (list-to-Monty-handler (rest M-list) 
                                        (Monty (first M-list) (Monty-door2 uMonty) (Monty-door3 uMonty)))
                        ]
    [(= (length M-list) 2) (list-to-Monty-handler (rest M-list)
                                        (Monty (Monty-door1 uMonty) (first M-list) (Monty-door3 uMonty)))
                        ]
    [(= (length M-list) 1) (Monty (Monty-door1 uMonty) (Monty-door2 uMonty) (first M-list))
                        ]
    [else (error "ERROR: list is not representative of choice")]
    ))

;;Monty-list -> Monty
(define (list-to-Monty M-list) (list-to-Monty-handler M-list (init-Monty)))

;; type, list -> list
(define (append-list item list)
  (reverse (cons item (reverse list)))
  )

;; int, list -> list of Montys
(define (random-lom-handler size list) 
  (cond
    [(= (length list) size) list]
    [else (random-lom-handler size (append-list (random-Monty) list))]
    ))

(define (random-lom size) (random-lom-handler size '()))
;;==================================================================================================
;;Monty tests

;;Test-Monty
(define test-Monty (random-Monty))

;;Test symmetry for list->Monty/Monty->list functions
(check-equal? (list-to-Monty (Monty-to-list test-Monty)) test-Monty)


;;Part 2: Defining Choice, a choice for each door
;;--------------------------------------------------------------------------------------------------

(struct Choice [chosen1 chosen2 chosen3] #:transparent)

;; -> Choice
(define (init-Choice) (Choice 0 0 0))

;; Choice -> list
(define (Choice-to-list uChoice) 
  (list (Choice-chosen1 uChoice) (Choice-chosen2 uChoice) (Choice-chosen3 uChoice)
        ))

;; Choice-list, Choice -> Choice
(define (list-to-Choice-handler C-list uChoice) 
  (cond
    [(= (length C-list) 3) (list-to-Choice-handler (rest C-list) 
                                        (Choice (first C-list) (Choice-chosen2 uChoice) (Choice-chosen3 uChoice)))
                        ]
    [(= (length C-list) 2) (list-to-Choice-handler (rest C-list)
                                        (Choice (Choice-chosen1 uChoice) (first C-list) (Choice-chosen3 uChoice)))
                        ]
    [(= (length C-list) 1) (Choice (Choice-chosen1 uChoice) (Choice-chosen2 uChoice) (first C-list))
                        ]
    [else (error "ERROR: list is not representative of choice")]
    ))

;; Choice-list -> Choice
(define (list-to-Choice C-list) (list-to-Choice-handler C-list (init-Choice)))

;; int, Choice-> Choice
(define (set-Choice index uChoice) 
  (cond
    [(= index 1) (Choice 1 0 0)]
    [(= index 2) (Choice 0 1 0)]
    [(= index 3) (Choice 0 0 1)]
    [else (error "ERROR: choice not contained in scenario!")]
    ))

;; ->Choice
(define (random-Choice) (set-Choice (random-door) (init-Choice)))

;; int, list -> list of Choices
(define (random-loc-handler size list) 
  (cond
    [(= (length list) size) list]
    [else (random-loc-handler size (append-list (random-Choice) list))]
    ))

(define (random-loc size) (random-loc-handler size '()))

;;==================================================================================================
;;Choice tests

;;Test-Monty
(define test-Choice (random-Choice))

;;Test symmetry for list->Monty/Monty->list functions
(check-equal? (list-to-Choice (Choice-to-list test-Choice)) test-Choice)


;;Part 3: Collection, an accumulator to report straight wins and switch wins
;;--------------------------------------------------------------------------------------------------
(struct Clct [strt switch] #:transparent)

;;->Clct
(define (init-Clct) (Clct 0 0))

;;Clct->Clct
(define (strt-win uClct)
  (Clct (add1 (Clct-strt uClct)) (Clct-switch uClct))
  )

;;Clct->Clct
(define (switch-win uClct)
  (Clct (Clct-strt uClct) (add1 (Clct-switch uClct)))
  )

;;Clct->int
(define (game-total uClct) (+ (Clct-strt uClct) (Clct-switch uClct)))

;;Clct->pClct (Percent Collection)
(define (report-stats uClct)
  (Clct (* 100 (/ (exact->inexact (Clct-strt uClct)) (game-total uClct)))
        (* 100 (/ (exact->inexact (Clct-switch uClct)) (game-total uClct)))
        ))

;;==================================================================================================
;;Collection tests

(define test-Collect (init-Clct))
(check-equal? test-Collect (Clct 0 0 ))
(check-equal? (strt-win test-Collect) (Clct 1 0 ))
(check-equal? (switch-win test-Collect) (Clct 0 1 ))

(define test-Collect2 (Clct 1 1 ))
(check-equal? (game-total test-Collect2) 2)
(check-equal? (report-stats test-Collect2) (Clct 50.0 50.0))

;;Part 4: Defining Gameshow, a representation of the choices and the doors
;;--------------------------------------------------------------------------------------------------

(struct Gameshow [Choice Monty] #:transparent)

;;Choice, Monty-> Gameshow
(define (set-Gameshow uChoice uMonty) (Gameshow uChoice uMonty))

;; ->Gameshow
(define (random-Gameshow) (Gameshow (random-Choice) (random-Monty)))

;; int, (empty) list -> list of Gameshows
(define (random-log-handler size list)
    (cond
    [(= (length list) size) list]
    [else (random-log-handler size (append-list (random-Gameshow) list))]
    ))

;; int -> list of Gameshows
(define (random-log size) (random-log-handler size '()))
;;==================================================================================================
;;Gameshow tests


;;Part 5: Host functions
;;--------------------------------------------------------------------------------------------------

;;Choice, Monty -> Gameshow
(define (Host-reveal-handler uChoice uMonty) 
  (cond
    [(zero? (+ (Choice-chosen1 uChoice) (Monty-door1 uMonty)))
     (Gameshow (Choice -1 (Choice-chosen2 uChoice) (Choice-chosen3 uChoice)) uMonty)]
    [(zero? (+ (Choice-chosen2 uChoice) (Monty-door2 uMonty)))
     (Gameshow (Choice (Choice-chosen1 uChoice) -1 (Choice-chosen3 uChoice)) uMonty)]
    [(zero? (+ (Choice-chosen3 uChoice) (Monty-door3 uMonty)))
     (Gameshow (Choice (Choice-chosen1 uChoice) (Choice-chosen2 uChoice) -1) uMonty)]
    [else (error "ERROR: Host could not find a goat!")]
    ))

;; Gameshow -> Gameshow
(define (Host-reveal uGameshow) 
  (Host-reveal-handler (Gameshow-Choice uGameshow) (Gameshow-Monty uGameshow)))
     
;;int, list-> list of Gameshows
(define (random-rlog-handler size list)
  (cond
    [(= (length list) size) list]
    [else (random-rlog-handler size (append-list (Host-reveal (random-Gameshow)) list))]
    ))

;;int -> list of Gameshows
(define (random-rlog size) (random-rlog-handler size '()))

;;==================================================================================================
;;Host tests

;;(define test-Gameshow (random-Gameshow))

;;(Host-reveal test-Gameshow)

;;Part 6: Evaluation Function
;;--------------------------------------------------------------------------------------------------

;; list of Choices, list of Montys, Collection -> Collection
(define (game-evaluation-handler Clist Mlist uClct)
  (cond
    [(= 1 (first Clist))
     (cond
       [(= 1 (first Mlist)) (strt-win uClct)]
       [(zero? (first Mlist)) (switch-win uClct)]
       [else (error "ERROR: Monty contains unknown door value.")]
       )]
    [(zero? (first Clist))
     (cond
       [(= 1 (first Mlist)) (switch-win uClct)]
       [(zero? (first Mlist)) (strt-win uClct)]
       [else (error "ERROR: Monty contains unknown door value.")]
       )]
    [(= -1 (first Clist))
     (cond
       [(zero? (first Mlist)) (game-evaluation-handler (rest Clist) (rest Mlist) uClct)]
       [else (error "ERROR: Host DID NOT choose a goat.")]
       )]
    [else (error "ERROR: List was either empty or did not contain correct information.")]
    ))

;;Gameshow, Collection -> Collection
(define (game-evaluation uGameshow uClct) 
  (game-evaluation-handler (Choice-to-list (Gameshow-Choice uGameshow)) (Monty-to-list (Gameshow-Monty uGameshow)) uClct))

;;revealed list of Gameshows, Collection -> Collection
(define (rlog-evaluation rlog uClct) 
  (cond
    [(empty? rlog) uClct]
    [else (rlog-evaluation (rest rlog) (game-evaluation (first rlog) uClct))]
    ))
;;==================================================================================================
;;Evaluation tests



;;Finale: Begin the simulation!
;;--------------------------------------------------------------------------------------------------

(define size 1000)

(fprintf (current-output-port)
           "Generating sample size of ~a\n"
           size)


(define test (random-rlog size))

;;test
;;^Uncomment to see the generated list

#|
     A generated list of "Gameshows"
         Each Gameshow is composed of a "Choice" and a "Monty"
     
     A Choice is three integers that represents the user's choice, and the host's reveal.
       A zero means the corresponding door was not chosen.
       A -1 means that this is the door the 'host' revealed as a goat.
       A 1 means that this is the door the user randomly chose.

     A Monty is representative of the three door scenario 
       A 1 indicates this is the door containing the car
       A zero indicates this is a door containing a goat.
|#


(define tClct (init-Clct))
#|
  A "Collection"
    Each Collection is composed of "strt-win" and "switch-win"

  A strt-win is when a player wins by keeping their current selection.

  A switch-win is when a player 'would have' won by switching their door.
|#

(define answer (rlog-evaluation test tClct))
#|
 rlog-evaluation evaluates each Gameshow, and updates the Collection.
      When the list has been emptied, it returns the Collection.
|#


(fprintf (current-output-port)
           "\nResults:\nYou would win ~a games by keeping your door.\nYou would win ~a games by switching.\n"
           (Clct-strt answer)
           (Clct-switch answer)
           )

(define percentages (report-stats answer))
#|
  Transforms Collection into percentage collection.
|#

(fprintf (current-output-port)
           "\nPercentages:\nYou would win ~a% of the time keeping your door.\nYou would win ~a% of the time by switching.\n"
           (Clct-strt percentages)
           (Clct-switch percentages)
           )


;;==================================================================================================
