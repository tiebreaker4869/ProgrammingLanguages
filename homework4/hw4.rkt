#lang racket

;; This line exports all of your defined functions,
;; so you can call them in hw4tests.rkt and so we can
;; call them in our tests.
;; Don't remove or comment out this line!
(provide (all-defined-out)) 

;; Implement your code below

;; #1
(define (sequence spacing low high)
  (if (> low high) null (cons low (sequence spacing (+ low spacing) high))))

;; keep going with the rest of the problems below
(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs)
  )

(define (list-nth-mod xs n)
  (cond
    [(< n 0) (error "list-nth-mod: negative number")]
    [(empty? xs) (error "list-nth-mod: empty list")]
    [#t (let* ([len (length xs)] [r (remainder n len)])
          (car (list-tail xs r)))])
  )

(define (stream-first-k-such-that f k s)
  (if (= k 0)
      '()
      (let* ([num-stream (s)]
             [hd (car num-stream)])
            (if (f hd)
                (cons hd (stream-first-k-such-that f (- k 1) (cdr num-stream)))
                (stream-first-k-such-that f k (cdr num-stream))))))

(define funny-number-stream
  (lambda ()
    (define (produce-numbers start)
      (cons (if (= (remainder start 6) 0) (- 0 start) start) (lambda () (produce-numbers (+ start 1)))))
    (produce-numbers 1)))

(define dan-then-dog (lambda () (cons "dan.jpg" (lambda () (cons "dog.jpg" dan-then-dog)))))

(define (stream-add-one s)
  (lambda () (let ([stream-pair (s)]) (cons (cons 1 (car stream-pair)) (stream-add-one (cdr stream-pair))))))

(define (cycle-lists xs ys)
  (lambda ()
    (define (produce-cycle-pairs n) (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) (lambda () (produce-cycle-pairs (+ n 1)))))
    (produce-cycle-pairs 0)))

(define (vector-assoc v vec)
  (let ([len (vector-length vec)])
    (begin
      (define (assoc-helper n)
        (cond
          [(>= n len) #f]
          [(not (pair? (vector-ref vec n))) (assoc-helper (+ n 1))]
          [(equal? (car (vector-ref vec n)) v) (vector-ref vec n)]
          [#t (assoc-helper (+ n 1))]))
      (assoc-helper 0))))

(define (caching-assoc xs n)
  (let ([cache (build-vector n (lambda (x) #f))]
        [counter 0])
    (begin
      (define (cached-assoc v)
        (let ([cache-res (vector-assoc v cache)])
          (if cache-res
              cache-res
              (let ([res-in-lst (assoc v xs)])
                (if (not res-in-lst)
                    res-in-lst
                    (begin
                      (vector-set! cache counter res-in-lst)
                      (set! counter (remainder (+ counter 1) n))
                      res-in-lst))))))
      cached-assoc))
  )