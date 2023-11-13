;; CSE341, Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct isgreater (e1 e2)    #:transparent) ;; if e1 > e2 then 1 else 0
(struct ifnz (e1 e2 e3) #:transparent) ;; if not zero e1 then e2 else e3
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair   (e1 e2) #:transparent) ;; make a new pair
(struct first   (e)     #:transparent) ;; get first part of a pair
(struct second  (e)     #:transparent) ;; get second part of a pair
(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e)     #:transparent) ;; if e1 is unit then 1 else 0

;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1

;; CHANGE (put your solutions here)

(define (racketlist->mupllist xs)
  (cond
    [(null? xs) (munit)]
    [#t (apair (car xs) (racketlist->mupllist (cdr xs)))]))

(define (mupllist->racketlist xs)
  (cond
    [(munit? xs) null]
    [#t (cons (apair-e1 xs) (mupllist->racketlist (apair-e2 xs)))]))

;; Problem 2

;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        [(isgreater? e)
         (let ([v1 (eval-under-env (isgreater-e1 e) env)]
               [v2 (eval-under-env (isgreater-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (int 1)
                   (int 0))
               (error "MUPL isgreater applied to non-number")))]
        [(ifnz? e) (let ([v1 (eval-under-env (ifnz-e1 e) env)])
                     (if (int? v1)
                         (if (= 0 (int-num v1))
                             (eval-under-env (ifnz-e3 e) env)
                             (eval-under-env (ifnz-e2 e) env))
                         (error "MUPL ifnz condition applied to non-number")))]
        [(apair? e) (let ([v1 (eval-under-env (apair-e1 e) env)]
                          [v2 (eval-under-env (apair-e2 e) env)])
                      (apair v1 v2))]
        [(first? e) (let ([v1 (eval-under-env (first-e e) env)])
                      (if (apair? v1)
                          (apair-e1 v1)
                          (error "MUPL first applied to non-pair")))]
        [(second? e) (let ([v1 (eval-under-env (second-e e) env)])
                       (if (apair? v1)
                           (apair-e2 v1)
                           (error "MUPL second applied to non-pair")))]
        [(munit? e) e]
        [(int? e) e]
        [(ismunit? e) (if (munit? (eval-under-env (ismunit-e e) env))
                          (int 1)
                          (int 0))]
        [(fun? e) (closure env e)]
        [(mlet? e) (let ([v1 (eval-under-env (mlet-e e) env)])
                     (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v1) env)))]
        [(call? e) (let ([v1 (eval-under-env (call-funexp e) env)]
                         [v2 (eval-under-env (call-actual e) env)])
                     (if (closure? v1)
                         (let ([extend-arg-env (cons (cons (fun-formal (closure-fun v1)) v2) (closure-env v1))])
                           (if (null? (fun-nameopt (closure-fun v1)))
                               (eval-under-env (fun-body (closure-fun v1)) extend-arg-env)
                               (eval-under-env (fun-body (closure-fun v1)) (cons (cons (fun-nameopt (closure-fun v1)) v1) extend-arg-env))))
                         (error "MUPL call applied to non-closure")))]
        ;; CHANGE add more cases here
        [#t (error (format "bad MUPL expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifmunit e1 e2 e3)
  (ifnz (ismunit e1) e2 e3))

(define (mlet* bs e2)
  (cond
    [(null? bs) e2]
    [#t (mlet (car (car bs)) (cdr (car bs)) (mlet* (cdr bs) e2))]))

(define (ifeq e1 e2 e3 e4)
  (mlet* (list (cons "_x" e1) (cons "_y" e2)) (ifnz (add (isgreater e1 e2) (isgreater e2 e1)) e4 e3)))

;; Problem 4

(define mupl-filter
  (fun "_mupl-filter" "_f"
       (fun "_filter" "_xs"
            (ifmunit (var "_xs")
                     (munit)
                     (ifnz (call (var "_f") (first (var "_xs")))
                           (apair (first (var "_xs")) (call (var "_filter") (second (var "_xs"))))
                           (call (var "_filter") (second (var "_xs"))))))))

(define mupl-all-gt
  (mlet "filter" mupl-filter
        (fun "_mupl-all-gt" "_n" (call (var "filter") (fun null "_num" (ifnz (isgreater (var "_num") (var "_n")) (int 1) (int 0)))))))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e)
  (cond
    [(var? e) e]
    [(int? e) e]
    [(add? e) (let ([e1-computed (compute-free-vars (add-e1 e))]
                    [e2-computed (compute-free-vars (add-e2 e))])
                (add e1-computed e2-computed))]
    [(isgreater? e) (let ([e1-computed (compute-free-vars (isgreater-e1 e))]
                          [e2-computed (compute-free-vars (isgreater-e2 e))])
                          (isgreater e1-computed e2-computed))]
    [(ifnz? e) (let ([e1-computed (compute-free-vars (ifnz-e1 e))]
                     [e2-computed (compute-free-vars (ifnz-e2 e))]
                     [e3-computed (compute-free-vars (ifnz-e3 e))])
                 (ifnz e1-computed e2-computed e3-computed))]
    [(fun? e)(begin
               (define (gather-vars expr)
                 (cond
                   [(var? expr) (set (var-string expr))]
                   [(int? expr) (set)]
                   [(add? expr) (set-union (gather-vars (add-e1 expr)) (gather-vars (add-e2 expr)))]
                   [(isgreater? expr) (set-union (gather-vars (isgreater-e1 expr)) (gather-vars (isgreater-e2 expr)))]
                   [(ifnz? expr) (set-union (gather-vars (ifnz-e1 expr)) (gather-vars (ifnz-e2 expr)) (gather-vars (ifnz-e3 expr)))]
                   [(fun? expr) (let ([free-vars-in-subexpr (gather-vars (fun-body expr))])
                                  (set-remove (set-remove free-vars-in-subexpr (fun-nameopt expr)) (fun-formal expr)))]
                   [(call? expr) (set-union (gather-vars (call-funexp expr)) (gather-vars (call-actual expr)))]
                   [(mlet? expr) (set-union (gather-vars (mlet-e expr)) (gather-vars (mlet-body expr)))]
                   [(apair? expr) (set-union (gather-vars (apair-e1 expr)) (gather-vars (apair-e2 expr)))]
                   [(first? expr) (gather-vars (first-e expr))]
                   [(second? expr) (gather-vars (second-e expr))]
                   [(munit? expr) (set)]
                   [(ismunit? expr) (gather-vars (ismunit-e expr))]
                   [#t (error "gather-vars on non-MUPL expression")]))
               (fun-challenge (fun-nameopt e) (fun-formal e) (fun-body e) (gather-vars e)))]
    [(call? e) (let ([funexp-computed (compute-free-vars (call-funexp e))]
                     [actual-computed (compute-free-vars (call-actual e))])
                 (call funexp-computed actual-computed))]
    [(mlet? e) (let ([e-computed (compute-free-vars (mlet-e e))]
                     [body-computed (compute-free-vars (mlet-body e))])
                     (mlet (mlet-var e) e-computed body-computed))]
    [(apair? e) (let ([e1-computed (compute-free-vars (apair-e1 e))]
                      [e2-computed (compute-free-vars (apair-e2 e))])
                  (apair e1-computed e2-computed))]
    [(first? e) (first (compute-free-vars (first-e e)))]
    [(second? e) (second (compute-free-vars (second-e e)))]
    [(munit? e) e]
    [(ismunit? e) (ismunit (compute-free-vars (ismunit-e e)))]
    [#t (error "compute-free-vars on non-MUPL expression")]))

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))

