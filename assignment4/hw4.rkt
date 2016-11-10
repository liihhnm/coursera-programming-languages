
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

(define my-stream
  (lambda () (cons 1 my-stream)))
;; put your code below
(define (sequence low high stride)
  (if (> low high)
      '()
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (string) (string-append string suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(empty? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              (car (list-tail xs i)))]))

;;return a list of first n items of stream s
(define (stream-for-n-steps s n)
  (if (= n 0)
      '()
      (cons (car (s)) (stream-for-n-steps (cdr (s)) (- n 1)))))

(define (funny-number first)
  (cons (if (= (remainder first 5) 0)
            (- first)
            first)
        (lambda () (funny-number (+ first 1)))))
(define funny-number-stream (lambda () (funny-number 1)))

(define (dan-dog items)
  (cons (car items) (lambda () (dan-dog (cons (cdr items) (car items))))))
(define dan-then-dog (lambda () (dan-dog (cons "dan.jpg" "dog.jpg"))))
  






  