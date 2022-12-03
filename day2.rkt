#lang racket

(define (get-input-as-list input)
  (for/list ([line (file->lines input)])
    line))

(define day-2-input-list (get-input-as-list "day2_input.txt"))

(define (get-value-of-letter chr)
  (cond
    [(or (equal? chr "A") (equal? chr "X")) 1]
    [(or (equal? chr "B") (equal? chr "Y")) 2]
    [(or (equal? chr "C") (equal? chr "Z")) 3]
    )
  )

(define (player-wins? player-val enemy-val)
  (if (or (equal? (- player-val  enemy-val) 1)
          (equal? (- player-val enemy-val) -2))
      #t
      #f)
  )

(define (player-draws? player-val enemy-val)
  (if (equal? player-val enemy-val) #t #f)
  )

(define (convert-strategy-to-list strategy-string)
  (string-split strategy-string)
  )

(define (problem-2-part-1-helper lst curr-count)
  (cond
         [(null? lst) curr-count]
         [else
            (let* ([strategy-list (convert-strategy-to-list (car lst))]
                   [player-val (get-value-of-letter (list-ref strategy-list 1))]
                   [enemy-val (get-value-of-letter (list-ref strategy-list 0))])
                  (cond 
                    [(player-draws? player-val enemy-val) (problem-2-part-1-helper (cdr lst) (+ (+ curr-count 3) player-val))]
                    [(player-wins? player-val enemy-val) (problem-2-part-1-helper (cdr lst) (+ (+ curr-count 6) player-val))]  
                    [else
                     (problem-2-part-1-helper (cdr lst) (+ curr-count player-val))
                    ])  
                )
         ])
  )

(define (problem-2-part-1 lst)
  (problem-2-part-1-helper lst 0)
  )

(define (get-players-value-with-condition enemy-val game-condition)
  (cond
    [(equal? game-condition "X") (if (equal? (- enemy-val 1) 0) 3 (- enemy-val 1))]
    [(equal? game-condition "Y") (+ enemy-val 3)]
    [(equal? game-condition "Z") (if (equal? (+ enemy-val 1) 4) 7 (+ (+ enemy-val 1) 6))]
  )
  )

(define (problem-2-part-2-helper lst curr-count)
  (cond
         [(null? lst) curr-count]
         [else
            (let* ([strategy-list (convert-strategy-to-list (car lst))]
                   [game-condition (list-ref strategy-list 1)]
                   [enemy-val (get-value-of-letter (list-ref strategy-list 0))]
                   )
              (problem-2-part-2-helper (cdr lst) (+ curr-count (get-players-value-with-condition enemy-val game-condition)))
             
                )
         ])
  )

(define (problem-2-part-2 lst)
  (problem-2-part-2-helper lst 0))
