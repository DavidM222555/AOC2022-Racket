#lang racket

(define (get-file-as-list filename)
  (for/list ([line (file->lines filename)])
    line)
  )

(define problem-3-input (get-file-as-list "day3_input.txt"))

(define (split-string-in-half str)
  (let* ([str-len (string-length str)]
        [mid-point (/ str-len 2)])
    (cons (substring str 0 mid-point) (substring str mid-point))))


(define (get-shared-letters lst-1 lst-2 shared-letter-lst)
  (cond [(null? lst-1) shared-letter-lst] ; By the problem statement we can assume that both lst-1 and lst-2 will be null at the same time
        [else
         (cond
           [(member (car lst-1) lst-2) (get-shared-letters (cdr lst-1) lst-2 (cons (car lst-1) shared-letter-lst))] ; The case where we have an equal letter
           [else (get-shared-letters (cdr lst-1) lst-2 shared-letter-lst)]
           )
         ]))

(define (get-priority-of-letter letter)
  (if (char-upper-case? letter) (- (char->integer letter) 38) (- (char->integer letter) 96) )
  )

(define (get-sum-of-shared-letters lst)
  (foldl + 0 (map get-priority-of-letter lst))
  )

(define (problem-3-part-1-helper lst running-sum)
  (cond [(null? lst) running-sum]
        [else
         (let* ([curr-rucksack (car lst)]
                [split-rucksack (split-string-in-half curr-rucksack)]
                [left-compartment (string->list (car split-rucksack))]
                [right-compartment (string->list (cdr split-rucksack))]
                [shared-letters (remove-duplicates (get-shared-letters left-compartment right-compartment '()))]
                )
           (problem-3-part-1-helper (cdr lst) (+ running-sum (get-sum-of-shared-letters shared-letters)))
           )
         ]
        )
  )

(problem-3-part-1-helper problem-3-input 0)

(define (group-input-by-three input-list)
  (for/list ([idx [in-range 0 (/ (length input-list) 3)]])
    ; Below we utilize the indexing scheme: idx * 3 + 0, idx * 3 + 1, idx * 3 + 2 to find groups of three.
    (cons (cons (list-ref input-list (* idx 3)) (list-ref input-list (+ (* idx 3) 1))) (list-ref input-list (+ (* idx 3) 2)))
    )
  )

(define (get-shared-letters-part-2 lst-1 lst-2 lst-3 shared-letter-lst)
  (cond [(null? lst-1) shared-letter-lst]
        [else
         (cond
           [(and (member (car lst-1) lst-2) (member (car lst-1) lst-3)) (get-shared-letters-part-2 (cdr lst-1) lst-2 lst-3 (cons (car lst-1) shared-letter-lst))]
           [else (get-shared-letters-part-2 (cdr lst-1) lst-2 lst-3 shared-letter-lst)]
           )
         ]
        )
  )


(define (problem-3-part-2-helper lst running-sum)
  (cond [(null? lst) running-sum]
        [else
         (let* ([curr-group (car lst)]
                [group-1 (string->list (car (car curr-group)))]
                [group-2 (string->list (cdr (car curr-group)))]
                [group-3 (string->list (cdr curr-group))]
                [shared-letters (remove-duplicates (get-shared-letters-part-2 group-1 group-2 group-3 '()))]
                )
           (problem-3-part-2-helper (cdr lst) (+ running-sum (get-priority-of-letter (car shared-letters))))
           )
         ]
        )
  )

(problem-3-part-2-helper (group-input-by-three problem-3-input) 0)

                                 