#lang plai

;[purpose] Type for the function definition, indicates function name, function call, id, or number
(define-type FAE
[num (n number?)]
[add (lhs FAE?) (rhs FAE?)]
[sub (lhs FAE?) (rhs FAE?)]
[id (name symbol?)]
[fun (param symbol?) (body FAE?)]
[app (fun-expr FAE?) (arg-expr FAE?)])

;[purpose] Type for numver and closure to store the enviornment
(define-type FAE-Value
[numV (n number?)]
[closureV (param symbol?)
(body FAE?)
(ds DefrdSub?)])

;[purpose] Deferred Substitution Cache; stores substituted values
(define-type DefrdSub
    [mtSub]
    [aSub (name symbol?) (value FAE-Value?) (ds DefrdSub?)])

;[contract] num-op: operators for arithmetic computation -> function for arithmetic computation -> numV
;[purpose] to get a function for arithmetic computation
;[test] (test ((num-op +) (numV 1) (numV 2)) (numV 3))
;[test] (test ((num-op -) (numV 2) (numV 1)) (numV 1))
(define (num-op op)
  (lambda (x y) (numV (op (numV-n x) (numV-n y))))) ; return function itself, contract will be the function

(test ((num-op +) (numV 1) (numV 2)) (numV 3))
(test ((num-op -) (numV 2) (numV 1)) (numV 1))

;[contract] num+: operators for arithmetic computation -> function for arithmetic computation -> numV
;[purpose] convert num+ into num-op function, which uses lambda expression
;[test] (num+ (numV 1) (numV 2))
;[test] (num+ (numV 2) (numV 3))
(define num+ (num-op +))

(test (num+ (numV 1) (numV 2)) (numV 3))
(test (num+ (numV 2) (numV 3)) (numV 5))

;[contract] num-: operators for arithmetic computation -> function for arithmetic computation -> numV
;[purpose] convert num- into num-op function, which uses lambda expression
;[test] (num+ (numV 1) (numV 2))
;[test] (num+ (numV 2) (numV 3))
(define num- (num-op -))

(test (num- (numV 1) (numV 2)) (numV -1))
(test (num- (numV 2) (numV 3)) (numV -1))

;[contract] lookup: symbol DefrdSub -> FAE-Value
; purpose: to get a value for the given identifier (symbol)
;[test] (test/exn (lookup 'a (mtSub)) "free identifier")
;(test (lookup 'a (aSub 'a (numV 10) (mtSub))) (numV 10))
;(test (lookup 'y (aSub 'x (numV 1) (aSub 'y (numV 4) (mtSub)))) (numV 4))

(define (lookup name ds)
      (type-case DefrdSub ds
            [mtSub       ()                  (error 'lookup "free identifier")]
            [aSub      (i v saved)      (if (symbol=? i name)
                                                                     v
                                                                     (lookup name saved))]))

(test/exn (lookup 'a (mtSub)) "free identifier")
(test (lookup 'a (aSub 'a (numV 10) (mtSub))) (numV 10))
(test (lookup 'y (aSub 'x (numV 1) (aSub 'y (numV 4) (mtSub)))) (numV 4))

; [contract] parse: sexp -> FAE
; [purpose]: to convert sexp to FAE
; [test] (test (parse '{{fun {x} {+ x 1}} 10})
                    ;(app (fun 'x (add (id 'x) (num 1))) (num 10)))
;(test (parse '(+ 10 20)) (add (numV 10) (numV 20)))

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
(test (parse '(- 10 20)) (sub (num 10) (num 20)))


; [contract] interp: FAE DefrdSub -> FAE-Value
; purpose: to get FAE-Value from FAE
; test: (test (interp (app (fun 'x (add (id 'x) (num 1))) (num 10)) (mtSub)) (numV 11))
; (test (interp (app (fun 'y (app (fun 'x (add (id 'y) (id 'x))) (num 10))) (num 10)) (mtSub)) (numV 20))
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

(test (interp (app (fun 'x (add (id 'x) (num 1))) (num 10)) (mtSub)) (numV 11))
(test (interp (app (fun 'y (app (fun 'x (add (id 'y) (id 'x))) (num 10))) (num 10)) (mtSub)) (numV 20))
; [contract] run: sexp -> FAE-Value
; purpose: to run parse and interp in once.
; test: (test (run '{with {y 10} { {fun {x} {+ y x}} 10} } (mtSub)) (numV 20))

;(test (run '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 6} {f 4}}}} (mtSub)) (numV 7))
;(test (run ' {with {y 5} {+ y {with {z 5} {+ y 2}}}} (mtSub)) (numV 12))
;(test/exn (run ' {with {y 5} {+ z {with {z 5} {+ y 2}}}} (mtSub)) "free identifier")

(define (run sexp ds)
     (interp (parse sexp) ds))

(test (run '{with {y 10} { {fun {x} {+ y x}} 10} } (mtSub)) (numV 20))
(test (run '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 6} {f 4}}}} (mtSub)) (numV 7))
(test (run ' {with {y 5} {+ y {with {z 5} {+ y 2}}}} (mtSub)) (numV 12))
(test/exn (run ' {with {y 5} {+ z {with {z 5} {+ y 2}}}} (mtSub)) "free identifier")

(parse '{with {x 3} {+ x 3}})