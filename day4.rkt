#lang racket

(define (get-input-as-list filename)
  (for/list ([line (file->lines filename)])
    line)
  )

(define day-4-input-list (get-input-as-list "day4_input.txt"))

(struct range (low high) #:prefab)

(define (range-from-string str)
  (let ([split-str (string-split str "-")])
        (range (string->number (list-ref split-str 0)) (string->number (list-ref split-str 1)))
    )
  )

(define (range-contained-in-other range-1 range-2)
  (or (and (<= (range-low range-1) (range-low range-2))
           (>= (range-high range-1) (range-high range-2)))
      (and (<= (range-low range-2) (range-low range-1))
           (>= (range-high range-2) (range-high range-1)))
      )
  )

(define (problem-4-part-1-helper input-list running-sum)
  (cond [(null? input-list) running-sum]
        [else 
  
         (let* ([comma-split-line (string-split (car input-list) ",")]
                [range-1 (range-from-string (list-ref comma-split-line 0))]
                [range-2 (range-from-string (list-ref comma-split-line 1))]
                )
           (if (range-contained-in-other range-1 range-2)
               (problem-4-part-1-helper (cdr input-list) (add1 running-sum))
               (problem-4-part-1-helper (cdr input-list) running-sum)
               ))
         ])
  )

(define (problem-4-part-1 input-list)
  (problem-4-part-1-helper input-list 0)
  )

(problem-4-part-1 day-4-input-list)

(define (ranges-overlap-at-all-helper range-1 range-2)
   (or (and (>= (range-low range-1)  (range-low range-2))  (<= (range-low range-1)  (range-high range-2)))
       (and (>= (range-high range-1) (range-low range-2))  (<= (range-high range-1) (range-high range-2))))
)

(define (ranges-overlap-at-all range-1 range-2)
  (or (ranges-overlap-at-all-helper range-1 range-2) (ranges-overlap-at-all-helper range-2 range-1)))

(define (problem-4-part-2-helper input-list running-sum)
  (cond [(null? input-list) running-sum]
        [else 
         (let* ([comma-split-line (string-split (car input-list) ",")]
                [range-1 (range-from-string (list-ref comma-split-line 0))]
                [range-2 (range-from-string (list-ref comma-split-line 1))]
                )
           (if (ranges-overlap-at-all range-1 range-2)
               (problem-4-part-2-helper (cdr input-list) (add1 running-sum))
               (problem-4-part-2-helper (cdr input-list) running-sum)
               ))
         ])
  )

(define (problem-4-part-2 input-list)
  (problem-4-part-2-helper input-list 0)
  )

(problem-4-part-2 day-4-input-list)

