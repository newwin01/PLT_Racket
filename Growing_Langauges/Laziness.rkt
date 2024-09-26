#lang plai

(define-type LFAE
[num (n number?)]
[add (lhs LFAE?) (rhs LFAE?)]
[sub (lhs LFAE?) (rhs LFAE?)]
[id (name symbol?)]
[fun (param symbol?) (body LFAE?)]
[app (fun-expr LFAE?) (arg-expr LFAE?)])

(define-type LFAE-Value
        [numV         (n number?)]
        [closureV    (param symbol?) (body LFAE?) (ds DefrdSub?)]
        [exprV          (expr LFAE?) (ds DefrdSub?)
                              (value (box/c (or/c false LFAE-Value?)))])

(define-type DefrdSub
    [mtSub]
    [aSub (name symbol?) (value LFAE-Value?) (ds DefrdSub?)])

(define (strict v)
    (type-case LFAE-Value v
        [exprV (expr ds v-box)
                     (if (not (unbox v-box))    ;; box contains #f? Then evaluate expr as needed.
                          (local [(define v (strict (interp expr ds)))]
                              (begin (set-box! v-box v)
                                           v))      ;; return v after evaluating it.
                          (unbox v-box))] ;; just unbox to return the value that was already evaluated once.
        [else v]))  ;; for numV or closureV

(define (num-op op x y)
    (numV (op (numV-n (strict x))
                        (numV-n (strict y)))))
(define (num+ x y) (num-op + x y))
(define (num- x y) (num-op - x y))


;(define (lookup name ds)
;(type-case DefrdSub ds
;[mtSub () (error 'lookup "no binding for identifier")]
;[aSub (bound-name bound-value rest-ds)
;(if (symbol=? bound-name name)
;bound-value
;(lookup name rest-ds))]))

(define (lookup name ds)
      (type-case DefrdSub ds
            [mtSub       ()                  (error 'lookup "free identifier")]
            [aSub      (i v saved)      (if (symbol=? i name)
                                                                     v
                                                                     (lookup name saved))]))

(define (parse sexp)
   (match sexp
        [(? number?)                (num sexp)]
        [(list '+ l r)              (add (parse l) (parse r))]
        [(list '- l r)              (sub (parse l) (parse r))]
        [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
        [(? symbol?)                (id sexp)]
        [(list 'fun (list p) b)                 (fun p (parse b))]
        [(list f a)                 (app (parse f) (parse a))]
        [else                       (error 'parse "bad syntax: ~a" sexp)]))


(test (parse '{{fun {x} {+ x 1}} 10})
                    (app (fun 'x (add (id 'x) (num 1))) (num 10)))

(define (interp lfae ds)
    (type-case LFAE lfae
        [num   (n)      (numV n)]
       [add    (l r)    (num+ (interp l ds) (interp r ds))]
       [sub    (l r)    (num- (interp l ds) (interp r ds))]
       [id       (s)     (lookup s ds)]
       [fun     (p b)  (closureV p b ds)]
       [app (f a)
             (local [(define f-val (strict (interp f ds)))
                                 (define a-val (exprV a ds (box #f)))]
                            (interp (closureV-body f-val)
                                         (aSub (closureV-param f-val)
                                                     a-val
                                                     (closureV-ds f-val))))]))



;(test (interp (with 'x (num 5) (add (id 'x) (id 'x)))) (num 10))

;(test (interp (parse '(fun {a} {+ a a}))) (fun 'a (add (id 'a) (id 'a))))
(interp (parse '{with {y 10} {fun {x} {+ y x}}}) (mtSub))

(interp(parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}}) (mtSub))