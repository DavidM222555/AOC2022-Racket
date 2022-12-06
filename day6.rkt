#lang racket

(define (string-is-marker? str)
  (equal? (length (remove-duplicates (string->list str))) (string-length str)))

(define (problem-6-part-1 input)
  (for/first ([i (in-range 0 (- (string-length input) 4))]
              #:when (string-is-marker? (substring input i (+ i 4))))
    (+ i 4)
    )
  )

(define (problem-6-part-2 input)
  (for/first ([i (in-range 0 (- (string-length input) 14))]
              #:when (string-is-marker? (substring input i (+ i 14))))
    (+ i 14)
    )
  )
