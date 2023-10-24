#lang plai

(define-type FAE
[num (n number?)]
[add (lhs FAE?) (rhs FAE?)]
[sub (lhs FAE?) (rhs FAE?)]
[id (name symbol?)]
[fun (param symbol?) (body FAE?)]
[app (fun-expr FAE?) (arg-expr FAE?)])

(define-type FAE-Value
[numV (n number?)]
[closureV (param symbol?)
(body FAE?)
(ds DefrdSub?)])

(define-type DefrdSub
    [mtSub]
    [aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

(define (num-op op) (lambda (x y) (numV (op (numV-n x) (numV-n y))))) ; return function itself, contract will be the function

(define num+ (num-op +))
(define num- (num-op -))


;; (define (num+ x y) (num (+ (num-n x) (num-n y))))
;; (define (num- x y) (num (- (num-n y) (num-n y))))



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

(define (interp fae ds)
    (type-case FAE fae
        [num   (n)      (numV n)]
       [add    (l r)    (num+ (interp l ds) (interp r ds))]
       [sub    (l r)    (num- (interp l ds) (interp r ds))]
       [id       (s)     (lookup s ds)]
       [fun     (p b)  (closureV p b ds)]
       [app    (f a)   (local [(define f-val (interp f ds))
                                      (define a-val (interp a ds))]
                               (interp (closureV-body f-val)
                                       (aSub (closureV-param f-val)
                                                          a-val
                                                          (closureV-ds f-val))))]))



;(test (interp (with 'x (num 5) (add (id 'x) (id 'x)))) (num 10))

;(test (interp (parse '(fun {a} {+ a a}))) (fun 'a (add (id 'a) (id 'a))))
;(test (interp (parse '{with {y 10} {fun {x} {+ y x}} }) (mtSub))
        ;(closureV 'x (add (id 'y) (id 'x))        (aSub 'y (numV 10) (mtSub))))
(interp (parse '{with {y 10} { {fun {x} {+ y x}} 10} }) (mtSub))
(interp (parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}}) (mtSub))
