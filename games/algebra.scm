;; AisleRiot - algebra.scm  -*-scheme-*- 

(use-modules (aisleriot interface) (aisleriot api))

;12 A234 9TJQ A59T 348Q A278 A47T 2468 257T 368J 369Q 56JQ 579J
;3 23TJ A58Q 4679
;6 A2JQ 249J 358T 4589 A67Q 367T
;6 A3TQ A49Q 349T 258J 267J 5678
;6 AAQQ 22JJ 33TT 4499 5588 6677
;12 AAAA QQQQ 2222 JJJJ 3333 TTTT 4444 9999 5555 8888 6666 7777

(define gamemodes
  (list
    (list (make-card 1 spade) (make-card 2 spade) (make-card 3 spade) (make-card 4 spade))
    (list (make-card 2 spade) (make-card 3 spade) (make-card 10 spade) (make-card 11 spade))
    (list (make-card 1 spade) (make-card 2 spade) (make-card 11 spade) (make-card 12 spade))
    (list (make-card 1 spade) (make-card 3 spade) (make-card 10 spade) (make-card 12 spade))
    (list (make-card 1 spade) (make-card 1 club) (make-card 12 spade) (make-card 12 club))
    (list (make-card 1 spade) (make-card 1 heart) (make-card 1 club) (make-card 1 diamond))
    ))
(def-save-var gamemode 2)

; The set up:

(define tableau '(6 7 8 9))
(define foundation '(1 2 3 4))
(define stock 0)
(define waste 5)

(define (new-game)
  (initialize-playing-area)
  (set-ace-low)

  (make-standard-deck)
  (shuffle-deck)

  (add-normal-slot '() 'stock)
  (add-blank-slot)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-normal-slot '() 'foundation)
  (add-carriage-return-slot)
  (add-normal-slot '() 'waste)
  (add-blank-slot)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)
  (add-extended-slot '() down 'tableau)

  (set-cards-algebra)
  (give-status-message)

  ; new-game should return the width and height of the playing field
  (list 6 3.1)
)

(define (get-gamemode) (list-ref gamemodes gamemode))

(define (set-cards-algebra)
  (set-cards! stock (delete-cards (get-gamemode) DECK))
  (for-each (lambda (slot card) (set-cards! slot (list (flip-card card)))) foundation (get-gamemode)))

(define (delete-cards cards deck)
  (if (null? cards) deck (delete-cards (cdr cards) (delete-card (car cards) deck))))

(define (delete-card card deck)
  (if (cards-eq? card (car deck))
    (cdr deck)
    (cons (car deck) (delete-card card (cdr deck)))))

(define (give-status-message)
  (set-statusbar-message (string-append (get-stock-no-string))))

(define (get-stock-no-string)
  (string-append (G_"Stock left:") " " 
                 (number->string (length (get-cards 0)))))

(define (button-pressed slot-id card-list)
  (or (and (> slot-id waste)
           (null? (cdr card-list))
           (empty-slot? waste))
      (= slot-id waste)))

(define (complete-transaction start-slot card-list end-slot)
  (move-n-cards! start-slot end-slot card-list)
  #t)

(define (button-released start-slot card-list end-slot) #t
  (if (droppable? start-slot card-list end-slot)
      (complete-transaction start-slot card-list end-slot) 
  #f)
  )

(define (droppable? start-slot card-list end-slot)
  (and (not (= start-slot end-slot))
       (or (and (member end-slot tableau)
                (= start-slot waste))
           (and (member end-slot foundation)
                (= 1 (length card-list))
                (is-foundation-droppable? foundation (get-gamemode) end-slot (car card-list))))))

(define (is-foundation-droppable? foundation gamemode end-slot card)
  (if (= end-slot (car foundation))
    (= (get-value card)
       (wrapped-add (get-value (get-top-card end-slot)) (get-value (car gamemode))))
    (is-foundation-droppable? (cdr foundation) (cdr gamemode) end-slot card)))

(define (wrapped-add a b)
  (let ((s (+ a b)))
    (if (> s 13) (- s 13) s)))

(define (button-clicked start-slot)
  (and (= start-slot stock)
       (empty-slot? waste)
       (flip-stock stock waste 0 1)))

(define (button-double-clicked start-slot)
  #f)

(define (get-hint)
  (list 0 (G_"No hint available right now")))

(define (game-won)
  (and (= 13 (length (get-cards 1)))
       (= 13 (length (get-cards 2)))
       (= 13 (length (get-cards 3)))
       (= 13 (length (get-cards 4)))))

(define (game-over)
  (give-status-message)
  (not (game-won)))

(define (card-number-to-string card)
  (let ((n (get-value card)))
    (cond ((and (> n 1) (< n 11)) (number->string n))
        ((= n 1) "A")
        ((= n 11) "J")
        ((= n 12) "Q")
        ((= n 13) "K")
        (#t "?"))))

(define (get-options)
  (append
    (list 'begin-exclusive)
    (map (lambda (i g)
      (list (format #f "~a-~a-~a-~a"
        (card-number-to-string (car g))
        (card-number-to-string (cadr g))
        (card-number-to-string (caddr g))
        (card-number-to-string (cadddr g))
        ) (equal? gamemode i)))
      (iota (length gamemodes) 0) gamemodes)
    (list 'end-exclusive)
  ))

(define (apply-options options)
  (apply-gamemode 0 (cdr options) gamemodes))

(define (apply-gamemode i options gamemodes)
  (if (cadr (car options))
    (set! gamemode i)
    (apply-gamemode (+ 1 i) (cdr options) (cdr gamemodes)))
  ; (set-cards-algebra)
  )

(define (timeout) #f)

(set-features droppable-feature)

(set-lambda new-game button-pressed button-released
  button-clicked button-double-clicked
  game-over game-won get-hint get-options apply-options
  timeout droppable?)
