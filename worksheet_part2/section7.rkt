#lang racket

; a larger arithmetic language with two kinds of values, booleans and numbers ; an expression is any of these:  
(struct const (int) #:transparent) ; int should hold a number  
(struct negate (e1) #:transparent) ; e1 should hold an expression that evaluates to a const
(struct add (e1 e2) #:transparent) ; e1, e2 should hold  expressions that evaluates to consts
(struct multiply (e1 e2) #:transparent) ;e1, e2 should hold  expressions that evaluates to consts
(struct bool (b) #:transparent) ; b should hold #t or #f  
(struct eq-num (e1 e2) #:transparent) ;e1, e2 should hold  expressions that evaluates to consts
(struct if-then-else (e1 e2 e3) #:transparent) ; e1, e2, e3 should hold  expressions, e1 should evaluate to a bool

(define negate2023 (negate (const 2023)))

(define add340and1 (add (const 340) (const 1)))

(define ten-eq-fifteen (if-then-else (eq-num (const 10) (const 15)) (bool #t) (bool #f)))

(define (eval-exp e)
  (cond
    [(const? e) e]
    [(bool? e) e]
    [(negate? e) (let [(val (eval-exp (negate-e1 e)))]
                   (if (const? val)
                       (const (- 0 (const-int val)))
                       (error "negate expects an expression that evaluates to integer")))]
    [(add? e) (let [(val1 (eval-exp (add-e1 e))) (val2 (eval-exp (add-e2 e)))]
                (if (and (const? val1) (const? val2))
                    (const (+ (const-int val1) (const-int val2)))
                    (error "add expects subexpressions to evaluate to integers")))]
    [(multiply? e) (let [(val1 (eval-exp (multiply-e1 e))) (val2 (eval-exp (multiply-e2 e)))]
                     (if (and (const? val1) (const? val2))
                         (const (* (const-int val1) (const-int val2)))
                         (error "multiply expects subexpressions to evaluate to integers")))]
    [(eq-num? e) (let [(val1 (eval-exp (eq-num-e1 e))) (val2 (eval-exp (eq-num-e2 e)))]
                   (if (and (const? val1) (const? val2))
                       (bool (= (const-int val1) (const-int val2)))
                       (error "eq-num expects subexpressions to evaluate to integers")))]
    [(if-then-else? e) (let [(bexp (eval-exp (if-then-else-e1 e)))]
                         (if (bool? bexp)
                             (if (bool-b bexp)
                                 (eval-exp (if-then-else-e2 e))
                                 (eval-exp (if-then-else-e3 e)))
                             (error "if condition should be bool")))]
    [#t (error "unsupported constructs")])
  )

