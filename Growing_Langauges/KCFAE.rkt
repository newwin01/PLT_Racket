#lang plai

;Task1
;Solved by myself: Y
;Time Taken: 1 hours


;[purpose] Type for the function definition, indicates function name, function call, id, or number
(define-type KCFAE
[num (n number?)]
[add (lhs KCFAE?) (rhs KCFAE?)]
[sub (lhs KCFAE?) (rhs KCFAE?)]
[mul (lhs KCFAE?) (rhs KCFAE?)]
[id (name symbol?)]
[fun (param symbol?) (body KCFAE?)]
[app (fun-expr KCFAE?) (arg-expr KCFAE?)]
[if0 (test-expr KCFAE?) (then-expr KCFAE?) (else-expr KCFAE?)]
[withcc (name symbol?) (body KCFAE?)]
)

;[purpose] Type for numver and closure to store the enviornment
(define-type KCFAE-Value
  [numV        (n number?)]
  [closureV  (p procedure?)]
  [contV     (c procedure?)])

;[purpose] Deferred Substitution Cache; stores substituted values
(define-type DefrdSub
    [mtSub]
    [aSub (name symbol?) (value KCFAE-Value?) (ds DefrdSub?)])

;[contract] num-op: operators for arithmetic computation -> function for arithmetic computation -> numV
;[purpose] to get a function for arithmetic computation
;[test] (test ((num-op +) (numV 1) (numV 2)) (numV 3))
;[test] (test ((num-op -) (numV 2) (numV 1)) (numV 1))
;(test ((num-op +) (numV 4) (numV 5)) (numV 9))
;(test ((num-op -) (numV 3) (numV 2)) (numV 1))
;(test ((num-op +) (numV 2) (numV 1)) (numV 3))

(define (num-op op)
  (lambda (x y) (numV (op (numV-n x) (numV-n y))))) ; return function itself, contract will be the function

(test ((num-op +) (numV 1) (numV 2)) (numV 3))
(test ((num-op -) (numV 2) (numV 1)) (numV 1))
(test ((num-op +) (numV 4) (numV 5)) (numV 9))
(test ((num-op -) (numV 3) (numV 2)) (numV 1))
(test ((num-op +) (numV 2) (numV 1)) (numV 3))

;[contract] num+: operators for arithmetic computation -> function for arithmetic computation -> numV
;[purpose] convert num+ into num-op function, which uses lambda expression
;[test] (num+ (numV 1) (numV 2))
;[test] (num+ (numV 2) (numV 3))
;(test (num+ (numV 2) (numV 2)) (numV 4))
;(test (num+ (numV 5) (numV 3)) (numV 8))
;(test (num+ (numV 8) (numV 2)) (numV 10))

(define num+ (num-op +))

(test (num+ (numV 1) (numV 2)) (numV 3))
(test (num+ (numV 2) (numV 3)) (numV 5))
(test (num+ (numV 2) (numV 2)) (numV 4))
(test (num+ (numV 5) (numV 3)) (numV 8))
(test (num+ (numV 8) (numV 2)) (numV 10))

;[contract] num-: operators for arithmetic computation -> function for arithmetic computation -> numV
;[purpose] convert num- into num-op function, which uses lambda expression
;[test] (num+ (numV 1) (numV 2))
;[test] (num+ (numV 2) (numV 3))
;(test (num- (numV 2) (numV 2)) (numV 0))
;(test (num- (numV 3) (numV 2)) (numV 1))
;(test (num- (numV 5) (numV 2)) (numV 3))

(define num- (num-op -))

(test (num- (numV 1) (numV 2)) (numV -1))
(test (num- (numV 2) (numV 3)) (numV -1))
(test (num- (numV 2) (numV 2)) (numV 0))
(test (num- (numV 3) (numV 2)) (numV 1))
(test (num- (numV 5) (numV 2)) (numV 3))

(define num* (num-op *))

(test (num* (numV 1) (numV 2)) (numV 2))
(test (num* (numV 2) (numV 3)) (numV 6))
(test (num* (numV 2) (numV 2)) (numV 4))
(test (num* (numV 3) (numV 2)) (numV 6))
(test (num* (numV 5) (numV 2)) (numV 25))


;[contract] lookup: symbol DefrdSub -> KCFAE-Value
; purpose: to get a value for the given identifier (symbol)
;[test] (test/exn (lookup 'a (mtSub)) "free identifier")
;(test (lookup 'a (aSub 'a (numV 10) (mtSub))) (numV 10))
;(test (lookup 'y (aSub 'x (numV 1) (aSub 'y (numV 4) (mtSub)))) (numV 4))
;(test/exn (lookup 'b (aSub 'a (numV 10) (mtSub))) "free identifier")
;(test (lookup 'b (aSub 'b (numV 10) (mtSub))) (numV 10))
;(test (lookup 'y (aSub 'x (numV 1) (aSub 'y (numV 19) (mtSub)))) (numV 19))

(define (lookup name ds)
      (type-case DefrdSub ds
            [mtSub       ()                  (error 'lookup "free identifier")]
            [aSub      (i v saved)      (if (symbol=? i name)
                                                                     v
                                                                     (lookup name saved))]))

(test/exn (lookup 'a (mtSub)) "free identifier")
(test (lookup 'a (aSub 'a (numV 10) (mtSub))) (numV 10))
(test (lookup 'y (aSub 'x (numV 1) (aSub 'y (numV 4) (mtSub)))) (numV 4))
(test/exn (lookup 'b (aSub 'a (numV 10) (mtSub))) "free identifier")
(test (lookup 'b (aSub 'b (numV 10) (mtSub))) (numV 10))
(test (lookup 'y (aSub 'x (numV 1) (aSub 'y (numV 19) (mtSub)))) (numV 19))

; [contract] parse: sexp -> KCFAE
; [purpose]: to convert sexp to KCFAE
; [test] (test (parse '{{fun {x} {+ x 1}} 10})
                    ;(app (fun 'x (add (id 'x) (num 1))) (num 10)))
;(test (parse '(+ 10 20)) (add (numV 10) (numV 20)))
;(test (parse '{with {x 3} {+ x 3}}) (app (fun 'x (add (id 'x) (num 3))) (num 3)))
;(test (parse '{withcc k {+ 1 {k 3}}}) (withcc 'k (add (num 1) (app (id 'k) (num 3)))))
;(test (parse 'x) (id 'x))

(define (parse sexp)
   (match sexp
        [(? number?)                (num sexp)]
        [(list '+ l r)              (add (parse l) (parse r))]
        [(list '- l r)              (sub (parse l) (parse r))]
        [(list '* l r)              (mul (parse l) (parse r))]
        [(list 'with (list i v) e) (app (fun i (parse e)) (parse v))]
        [(? symbol?)                (id sexp)]
        [(list 'fun (list p) b)                 (fun p (parse b))]
        [(list f a)                 (app (parse f) (parse a))]
        [(list 'withcc idexp kc)       (withcc idexp (parse kc))]
        [else                       (error 'parse "bad syntax: ~a" sexp)]))


(test (parse '{{fun {x} {+ x 1}} 10})
                    (app (fun 'x (add (id 'x) (num 1))) (num 10)))
(test (parse '(- 10 20)) (sub (num 10) (num 20)))
(test (parse '{with {x 3} {+ x 3}}) (app (fun 'x (add (id 'x) (num 3))) (num 3)))
(test (parse '{withcc k {+ 1 {k 3}}}) (withcc 'k (add (num 1) (app (id 'k) (num 3)))))
(test (parse 'x) (id 'x))


; [contract] numzero? :  KCFAE-Value -> boolean
; [purpose]: check the numV value is 0 or not
; [test] (test (numzero? (numV 0)) #t)
;(test (numzero? (numV 1)) #f)
;(test (numzero? (numV 2)) #f)
;(test (numzero? (numV -1)) #f)
;(test (numzero? (numV -2)) #f)

(define (numzero? n)
    (zero? (numV-n n)))

(test (numzero? (numV 0)) #t)
(test (numzero? (numV 1)) #f)
(test (numzero? (numV 2)) #f)
(test (numzero? (numV -1)) #f)
(test (numzero? (numV -2)) #f)

; [contract] interp: KCFAE DefrdSub -> KCFAE-Value
; purpose: to get KCFAE-Value from KCFAE
; test: (test (interp (app (fun 'x (add (id 'x) (num 1))) (num 10)) (mtSub)) (numV 11))
; (test (interp (app (fun 'y (app (fun 'x (add (id 'y) (id 'x))) (num 10))) (num 10)) (mtSub)) (numV 20))
;(test (interp (sub (num 10) (num 20)) (mtSub) (lambda (x) x)) (numV -10))
;(test (interp (withcc 'k (add (num 1) (app (id 'k) (num 3)))) (mtSub) (lambda (x) x)) (numV 3))
;(test (interp (app (fun 'x (add (id 'x) (num 3))) (num 3)) (mtSub) (lambda (x) x)) (numV 6))


(define (interp kcfae ds k)
    (type-case KCFAE kcfae
       [num   (n)      (k (numV n))]
       [add    (l r)    (interp l ds
                                (lambda (lv)
                                  (interp r ds
                                          (lambda (rv)
                                            (k (num+ lv rv))))))]
       [sub    (l r)    (interp l ds
                                (lambda (lv)
                                  (interp r ds
                                          (lambda (rv)
                                            (k (num- lv rv))))))]
      [mul    (l r)    (interp l ds
                                (lambda (lv)
                                  (interp r ds
                                          (lambda (rv)
                                            (k (num* lv rv))))))]
       [id       (s)     (k (lookup s ds))]
       [fun     (p b)  (k (closureV (lambda (a-val dyn-k)
                                      (interp b (aSub p a-val ds) dyn-k))))]
       [app (f a)   (interp f ds
                          (lambda (f-val)
                            (interp a ds
                                    (lambda (a-val)
                                      (type-case KCFAE-Value f-val
                                        [closureV (c) (c a-val k)]
                                        [contV (c) (c a-val)]
                                        [else (error "not an applicable value")])
                                      ))))]

      [if0 (test t f) (interp test ds
                              (lambda (tv) (if(eq? (interp test ds k) (numV 0))
                                              (interp t ds k) (interp f ds k))))]
      [withcc (cont-var body)
                    (interp body
                            (aSub cont-var
                                  (contV (lambda (val)
                                           (k val)))
                                  ds)
                            k)]
      )
  )

(test (interp (app (fun 'x (add (id 'x) (num 1))) (num 10)) (mtSub) (lambda (x) x)) (numV 11))
(test (interp (app (fun 'y (app (fun 'x (add (id 'y) (id 'x))) (num 10))) (num 10)) (mtSub) (lambda (x) x)) (numV 20))
(test (interp (sub (num 10) (num 20)) (mtSub) (lambda (x) x)) (numV -10))
(test (interp (withcc 'k (add (num 1) (app (id 'k) (num 3)))) (mtSub) (lambda (x) x)) (numV 3))
(test (interp (app (fun 'x (add (id 'x) (num 3))) (num 3)) (mtSub) (lambda (x) x)) (numV 6))


; [contract] run: sexp DefrdSub -> KCFAE-Value
; purpose: to run parse and interp in once.
; test: (test (run '{with {y 10} { {fun {x} {+ y x}} 10} } (mtSub)) (numV 20))

;(test (run '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 6} {f 4}}}} (mtSub)) (numV 7))
;(test (run ' {with {y 5} {+ y {with {z 5} {+ y 2}}}} (mtSub)) (numV 12))
;(test/exn (run ' {with {y 5} {+ z {with {z 5} {+ y 2}}}} (mtSub)) "free identifier")
;(test (run '{withcc k {+ 1 {k 3}}} (mtSub)) (numV 3))

;(test (run '{withcc done                                        ;; done = {fun {x}  x}
 ;          {{withcc esc                              ;; esc = {fun {y} {y 3}}
  ;                   {done {+ 1 {withcc k      ;; k = {fun {z} {{done {+ 1 z}} 3}}
   ;                                 {esc k}}}}}
    ;           3}} (mtSub)) (numV 4))

(define (run sexp ds)
     (interp (parse sexp) ds (lambda (x) x)))

(test (run '{with {y 10} { {fun {x} {+ y x}} 10} } (mtSub)) (numV 20))
(test (run '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 6} {f 4}}}} (mtSub)) (numV 7))
(test (run ' {with {y 5} {+ y {with {z 5} {+ y 2}}}} (mtSub)) (numV 12))
(test/exn (run ' {with {y 5} {+ z {with {z 5} {+ y 2}}}} (mtSub)) "free identifier")

(test (run '{withcc done                                        ;; done = {fun {x}  x}
           {{withcc esc                              ;; esc = {fun {y} {y 3}}
                     {done {+ 1 {withcc k      ;; k = {fun {z} {{done {+ 1 z}} 3}}
                                    {esc k}}}}}
               3}} (mtSub)) (numV 4))

; Unique Test cases

(test (run '{withcc k {+ 1 {k 3}}} (mtSub)) (numV 3))

(test (run '{{withcc k
               {k {fun {dummy} 3}}}
       1729} (mtSub)) (numV 3))

(test (run '{withcc k {+ 10 {with {x 3} {+ x 5}}}} (mtSub)) (numV 18))

(test (run '{withcc k {with {x {with {x 10} {+ x {k 3}}}} {+ x 3}}} (mtSub)) (numV 3))

(test (run '{withcc k {with {x {with {x 10} {withcc k2 {k2 3}}}} {+ x 3}}} (mtSub)) (numV 6))

(test (run '{+ {withcc k {with {x {with {y 6} {withcc k2 {k2 3}}}} {+ x 3}}} 10} (mtSub)) (numV 16))

(test (run '{withcc k {+ 1 {k 2}}} (mtSub)) (numV 2))

;continuation concept
(define number-producer
(local ([define resume (box false)])
(lambda (real-send)
(local ([define send (lambda (value-to-send)
(let/cc k
(begin
(set-box! resume k)
(real-send value-to-send))))])
(if (unbox resume)
((unbox resume) 'dummy)
(begin
(send 1)
(send 2)
(send 3)))))))

(define (get producer)
(let/cc k (producer k)))