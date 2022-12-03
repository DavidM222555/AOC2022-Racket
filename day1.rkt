#lang racket

(define (get-input-as-list input-file)
  (for/list ([line (file->lines input-file)])
    line
    )
  )


(define day-1-input (get-input-as-list "day1_input.txt"))

(define (day-1-problem-1 lst running-sum highest-sum)
  (cond ((null? lst) highest-sum)
        (else
         (cond
           ((equal? (car lst) "") (if (> running-sum highest-sum) (day-1-problem-1 (cdr lst) 0 running-sum)
                                      (day-1-problem-1 (cdr lst) 0 highest-sum))
                                  )
           (else
            (day-1-problem-1 (cdr lst) (+ running-sum (string->number (car lst))) highest-sum))))))


(define (day-1-part-2-helper lst sums running-sum)
    (take (sort (cond ((null? lst) sums)
        (else
         (cond
           ((equal? (car lst) "") (day-1-part-2-helper (cdr lst) (cons running-sum sums) 0))
           (else
            (day-1-part-2-helper (cdr lst) sums (+ running-sum (string->number (car lst)))))))) > ) 3))

(define (day-1-part-2)
  (foldl + 0 (day-1-part-2-helper day-1-input '() 0)))
