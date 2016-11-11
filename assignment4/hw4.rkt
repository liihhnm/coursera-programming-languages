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

(define (stream-add-zero s)
  (letrec ([assist (lambda (s) (cons (cons 0 (car (s)))
                                  (lambda () (assist (cdr (s))))))])
    (lambda () (assist s))))

(define (cycle-lists xs ys)
  (letrec ([assist (lambda (n) (cons (cons (list-nth-mod xs n)
                                           (list-nth-mod ys n))
                                     (lambda () (assist (+ n 1)))))])
    (lambda () (assist 0))))

(define (vector-assoc v vec)
  (letrec ([assist (lambda (n) (cond
                                 [(= n (vector-length vec)) #f]
                                 [(not (pair? (vector-ref vec n))) (assist (+ n 1))]
                                 [#t (if (equal? v (car (vector-ref vec n)))
                                         (vector-ref vec n)
                                         (assist (+ n 1)))]))])
    (assist 0)))

;before seeing the answer, have no idea about the exist of function (vector-assoc v vector)
;and not see it in document
(define (cached-assoc xs n)
  (letrec ([position 0]
           [cache (make-vector n (cons #f #f))]
           [f (lambda (v) (let ([ans (assoc v (vector->list cache))])
                            (if ans
                                ans
                                (let ([new-ans (assoc v xs)])
                                  (if (not new-ans)
                                      new-ans
                                      (begin (vector-set! cache position new-ans)
                                             (set! position (remainder (+ position 1) n))
                                             new-ans))))))])
    f))






  