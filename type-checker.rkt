#lang plait

(print-only-errors #t)

(define-type Expr
  (num [n : Number])
  (id [x : Symbol])
  (bool [b : Boolean])
  (bin-num-op [op : (Number Number -> Number)] [lhs : Expr] [rhs : Expr])
  (iszero [e : Expr])
  (bif [test : Expr] [then : Expr] [else : Expr])
  (with [bound-x : Symbol] [bound-body : Expr] [body : Expr])
  (fun [arg-x : Symbol] [arg-type : Type] [body : Expr] [body-type : Type])
  (app [fun : Expr] [arg : Expr])
  (nempty)
  (ncons [first : Expr] [rest : Expr])
  (nfirst [e : Expr])
  (nrest [e : Expr])
  (isnempty [e : Expr]))

(define-type Type
  (t-num)
  (t-bool)
  (t-nlist)
  (t-fun [arg : Type] [result : Type]))

(define-type Env
  (environ [name : Symbol] [type : Type] [env : Env])
  (noenv))

(is-operator : (S-Exp -> Boolean))
(define (is-operator op)
  (cond
    [(equal? `+ op) #t]
    [(equal? `- op) #t]
    [(equal? `* op) #t]
    [else #f]))

(define (get-operator op)
  (cond
    [(equal? `+ op) +]
    [(equal? `- op) -]
    [(equal? `* op) *]
    [else (error 'get-operator "wrong operator.")]))

(define (get-type tp arg-type)
  (cond
    [(equal? `(t-num) tp) (t-num)]
    [(equal? `(t-bool) tp) (t-bool)]
    [(equal? `(t-nlist) tp) (t-nlist)]
    [else (error 'type "error")]))
  
(parse : (S-Exp -> Expr))
(define (parse s-exp)
  (cond
    [(s-exp-number? s-exp) (num (s-exp->number s-exp))] ; number
    [(equal? `nempty s-exp) (nempty)] ; nempty
    [(s-exp-symbol? s-exp) (id (s-exp->symbol s-exp))] ; id
    [(s-exp-boolean? s-exp) (bool (s-exp->boolean s-exp))] ; boolean
    [(s-exp-list? s-exp)
       (cond
         [(is-operator (first (s-exp->list s-exp))) (bin-num-op (get-operator (first (s-exp->list s-exp))) (parse (second (s-exp->list s-exp))) (parse (third (s-exp->list s-exp))))] 
         [(equal? `iszero (first (s-exp->list s-exp))) (iszero (parse (second (s-exp->list s-exp))))] ; iszero
         [(equal? `bif (first (s-exp->list s-exp))) (bif (parse (second (s-exp->list s-exp))) (parse (third (s-exp->list s-exp))) (parse (fourth (s-exp->list s-exp))))] ; bif
         [(equal? `with (first (s-exp->list s-exp))) (with (s-exp->symbol (first (s-exp->list (second (s-exp->list s-exp))))) (parse (second (s-exp->list (second (s-exp->list s-exp))))) (parse (third (s-exp->list s-exp))))] ; with
         [(equal? `ncons (first (s-exp->list s-exp))) (ncons (parse (second (s-exp->list s-exp))) (parse (third (s-exp->list s-exp))))]
         [(equal? `nfirst (first (s-exp->list s-exp))) (nfirst (parse (second (s-exp->list s-exp))))]
         [(equal? `nrest (first (s-exp->list s-exp))) (nrest (parse (second (s-exp->list s-exp))))]
         [(equal? `isnempty (first (s-exp->list s-exp))) (isnempty (parse (second (s-exp->list s-exp))))]
         ;[(equal? `app (first (s-exp->list s-exp))) 
         [(equal? `fun (first (s-exp->list s-exp))) (if (equal? (get-type (first (reverse (s-exp->list s-exp))) (t-num)) (type-of (parse (fourth (s-exp->list s-exp)))))
                                                        (fun (s-exp->symbol (second (s-exp->list s-exp))) (get-type (third (s-exp->list s-exp)) (t-num)) (parse (fourth (s-exp->list s-exp))) (type-of (parse (fourth (s-exp->list s-exp)))))
                                                        (error 'type-of "type does not match."))]
         [else (error 'type-of "Invalid.")])]     
    [else (error 'parse "Invalid input.")]))

(define (env-helper x env)
  (type-case Env env
    [(noenv) (error 'env "No bound.")]
    [(environ name type env) (if (equal? x name)
                                 type
                                 (env-helper x env))]))

(define (type-of exp)
  (type-of-helper exp (noenv)))

(define (type-of-helper exp env)
  (type-case Expr exp
    [(num n) (t-num)]
    [(bool b) (t-bool)]
    [(id x) (env-helper x env)]
    [(bin-num-op op lhs rhs) (if (and (t-num? (type-of-helper lhs env))
                                      (t-num? (type-of-helper rhs env)))
                                 (t-num)
                                 (error 'type-of "bin-num-op should have two numbers."))]
    [(iszero e) (if (equal? (t-num) (type-of-helper e env))
                    (t-bool)
                    (error 'type-of "iszero should consume a number."))]
    [(bif test then el) (if (t-bool? (type-of-helper test env))
                              (if (equal? (type-of-helper then env) (type-of-helper el env))
                                  (type-of-helper then env)
                                  (error 'type-of "then and else have different type"))
                              (error 'type-of "test is not a boolean type"))]
    [(with bound-x bound-body body) (let [(this-env (environ bound-x (type-of-helper bound-body env) env))] (type-of-helper body this-env))]
    [(nempty) (t-nlist)]
    
    [(fun arg-x arg-type body body-type) (if (equal? body-type (type-of-helper body (environ arg-x arg-type env)))
                                             (t-fun arg-type body-type)
                                             (error 'type-of "type doesn't match"))]
    ;[(app fun arg) (if (t-fun? (type-of-helper fun env))
     ;                  (if (equal? (fun-arg-type fun) (type-of-helper arg env))
      ;                     (fun-body-type fun)
       ;                    (error 'type-of "type does not match"))
        ;               (error 'type-of "not a function"))]
    [(app fun arg) (t-num)]
    [(ncons first rest) (if (equal? (t-num) (type-of-helper first env))
                            (if (equal? (t-nlist) (type-of-helper rest env))
                                (t-nlist)
                                (error 'type-of "rest is not a list"))
                            (error 'type-of "first is not a number"))]
    [(nfirst e) (if (equal? (t-nlist) (type-of-helper e env))
                    (t-num)
                    (error 'type-of "e is not a list"))]
    [(nrest e) (if (equal? (t-nlist) (type-of-helper e env))
                   (t-nlist)
                   (error 'type-of "e is not a list"))]
    [(isnempty e) (if (equal? (t-nlist) (type-of-helper e env))
                      (t-bool)
                      (error 'type-of "e is not a list"))]))
                    
                            
                                    
(test (type-of (parse `13)) (t-num))
(test (type-of (parse `(+ 1 6))) (t-num))
(test (type-of (parse `(* 4 2))) (t-num))
(test (type-of (parse `(- 13 222))) (t-num))
(test/exn (type-of (parse `(1 2))) "Invalid")

(test (type-of (parse `(iszero 0))) (t-bool))
(test (type-of (parse `(iszero 3))) (t-bool))
(test/exn (type-of (parse `(iszero #t))) "iszero should consume a number.")

(test (type-of (parse `(bif #t #t #f))) (t-bool))
(test/exn (type-of (parse `(bit #t (+ 1 2) #f))) "Invalid.")

(test (type-of (parse `(with (x 3) (+ x 1)))) (t-num))
(test (type-of (parse `(with (x (+ 1 1)) #t))) (t-bool))

(test (type-of (parse `(fun x (t-num) (+ 1 1) (t-num)))) (t-fun (t-num) (t-num)))
(test (type-of (parse `(fun y (t-bool) (bif (iszero 0) #t #f) (t-bool)))) (t-fun (t-bool) (t-bool)))
(test/exn (type-of (parse `(fun k (t-nlist) (+ 1 3) (t-bool)))) "type does not match.")

(test (type-of (parse `nempty)) (t-nlist))
(test (type-of (parse `(ncons 1 nempty))) (t-nlist))
(test/exn (type-of (parse `(ncons 1 2))) "rest is not a list")

(test (type-of (parse `(nfirst nempty))) (t-num))
(test/exn (type-of (parse `(nfirst 1))) "e is not a list")
(test (type-of (parse `(nrest nempty))) (t-nlist))
(test/exn (type-of (parse `(nrest 1))) "e is not a list")




















