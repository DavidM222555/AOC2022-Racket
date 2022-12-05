#lang racket

(define (get-lines-of-input filename)
  (for/list ([line (file->lines filename)])
    line)
  )

(define day-5-list (get-lines-of-input "day5_input.txt"))

(define (get-input-until-instructions input-list curr-list)
  (if (equal? (car input-list) "") curr-list
      (get-input-until-instructions (cdr input-list) (cons (car input-list) curr-list))
      )
  )

(define input-header (get-input-until-instructions day-5-list '()))
(define top-row (string-trim (car input-header)))
(define top-row-delimited (string-split top-row "   "))
(define number-of-cols (length top-row-delimited))
(define stack-strings (cdr input-header))

(define (create-stack-object count stack-lst)
  (if (equal? count number-of-cols) stack-lst (create-stack-object (add1 count) (cons '() stack-lst))))

(define (get-move-strings input-lst move-lst)
  (cond [(null? input-lst) move-lst]
        [else
         (let* ([line (car input-lst)]
               [split-line (string-split line)])
           (if (and (>= (length split-line) 1) (equal? (list-ref split-line 0) "move"))
               (get-move-strings (cdr input-lst) (cons line move-lst))
               (get-move-strings (cdr input-lst) move-lst)
               ))]
        )
  )

(define move-strings (reverse (get-move-strings day-5-list '())))

(define stacks (create-stack-object 0 '()))

(define (get-stack-string-as-list stack-string)
  (string-split stack-string " ")
  )

(define (add-item-to-2D-list 2D-lst idx-to-add-at curr-idx item-to-add)
  (if (equal? curr-idx (length 2D-lst)) '()
      (if (equal? idx-to-add-at curr-idx)
          (append (list (append (list (list-ref 2D-lst curr-idx)) item-to-add)) (add-item-to-2D-list 2D-lst idx-to-add-at (add1 curr-idx) item-to-add))
          (append (list (list-ref 2D-lst curr-idx)) (add-item-to-2D-list 2D-lst idx-to-add-at (add1 curr-idx) item-to-add))
          )
      )
  )

(define (populate-stacks-from-string-list-helper str-lst stack-lst str-idx stack-idx)
  (if (>= str-idx (length str-lst)) stack-lst (cond
                                    [(> (string-length (list-ref str-lst str-idx)) 0)
                                     (populate-stacks-from-string-list-helper str-lst (add-item-to-2D-list stack-lst stack-idx 0 (list-ref str-lst str-idx)) (add1 str-idx) (add1 stack-idx))
                                     ]
                                    [else
                                     (populate-stacks-from-string-list-helper str-lst stack-lst (+ str-idx 4) (add1 stack-idx))
                                     ]
                                    )
      )
  )

(define (modify-string-list-if-necessary str-lst)
  (if (equal? (list-ref str-lst 0) "") (append (list "A") (remove "" (remove "" (remove "" str-lst)))) str-lst)
  )

(define (populate-stacks-from-string-list str-lst stack-lst)
  (populate-stacks-from-string-list-helper str-lst stack-lst 0 0)
  )

(define (populate-stacks stack-string-lst stacks)
  (if (null? stack-string-lst) stacks
      (populate-stacks (cdr stack-string-lst) (populate-stacks-from-string-list (modify-string-list-if-necessary (get-stack-string-as-list (car stack-string-lst))) stacks))
      )
  )

(define (flatten-lists populated-stacks)
  (for/list ([stk populated-stacks])
    (reverse (remove "A" (flatten stk)))
    )
  )

(define flattened-lists (flatten-lists (populate-stacks stack-strings stacks)))

(define stack-to-use flattened-lists)

(define (get-piece-on-top-of-stack stack stack-number)
  (list-ref (list-ref stack stack-number) 0)
  )

(define (move-piece-helper stack from to piece-to-move curr-idx)
  (cond
    [(equal? curr-idx (length stack)) '()]
    [(equal? curr-idx from)  (cons (cdr (list-ref stack from)) (move-piece-helper stack from to piece-to-move (add1 curr-idx)))]
    [(equal? curr-idx to) (cons (cons piece-to-move (list-ref stack curr-idx)) (move-piece-helper stack from to piece-to-move (add1 curr-idx)))]
    [else (cons (list-ref stack curr-idx) (move-piece-helper stack from to piece-to-move (add1 curr-idx)))]
    )
  )
  
(define (move-piece stack from to)
  (move-piece-helper stack from to (get-piece-on-top-of-stack stack from) 0))


(define (move-piece-by-value stack from to val)
  (move-piece-helper stack from to val 0))

(define (move-piece-multiple-times-helper stack from to number-of-times-to-move count)
  (if (equal? number-of-times-to-move count) stack
      (move-piece-multiple-times-helper (move-piece stack from to) from to number-of-times-to-move (add1 count))
      )
  )

(define (move-piece-multiple-times stack from to number-of-times-to-move)
  (move-piece-multiple-times-helper stack from to number-of-times-to-move 0)
  )


(define (problem-5-part-1 stack-to-modify move-lst)
  (cond [(null? move-lst) stack-to-modify]
        [else
         (let* ([split-str (string-split (car move-lst))]
                [num-to-move (string->number (list-ref split-str 1))]
                [from (- (string->number (list-ref split-str 3)) 1)]
                [to (- (string->number (list-ref split-str 5)) 1)]
                )
           (problem-5-part-1 (move-piece-multiple-times stack-to-modify from to num-to-move) (cdr move-lst))
           )
         ]
        )
  )

(problem-5-part-1 stack-to-use move-strings)


(define (get-top-n-crates-helper n stack count)
  (if (equal? n count) '() (cons (list-ref stack count) (get-top-n-crates-helper n stack (add1 count)))
      )
  )


(define (get-top-n-crates n stack stack-idx)
  (reverse (get-top-n-crates-helper n (list-ref stack stack-idx) 0)))


(define (move-list-of-values stack values from to)
  (if (null? values) stack
      (move-list-of-values (move-piece-by-value stack from to (car values)) (cdr values) from to ))
  )

(define (problem-5-part-2 stack-to-modify move-lst)
  (cond [(null? move-lst) stack-to-modify]
        [else
         (let* ([split-str (string-split (car move-lst))]
                [num-to-move (string->number (list-ref split-str 1))]
                [from (- (string->number (list-ref split-str 3)) 1)]
                [to (- (string->number (list-ref split-str 5)) 1)]
                )
           (problem-5-part-2 (move-list-of-values stack-to-modify (get-top-n-crates num-to-move stack-to-modify from) from to) (cdr move-lst))
           )
         ]
        )
  )

(problem-5-part-2 stack-to-use move-strings)