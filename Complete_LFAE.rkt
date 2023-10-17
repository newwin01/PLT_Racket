#lang plai

; Type definition for abstract syntax tree of LFAE
(define-type LFAE
    [num    (n number?)]
    [add     (lhs LFAE?) (rhs LFAE?)]
    [sub     (lhs LFAE?) (rhs LFAE?)]
    [id      (name symbol?)]
    [fun      (param symbol?) (body LFAE?)]
    [app     (ftn LFAE?) (arg LFAE?)])
  
        
; parse: sexp -> LFAE
; purpose: to convert sexp to LFAE
(define (parse sexp)
   (match sexp
        [(? number?)                (num sexp)]
        [(list '+ l r)              (add (parse l) (parse r))]
        [(list '- l r)              (sub (parse l) (parse r))]
        [(list 'with (list i v) e)  (app (fun i (parse e)) (parse v))]
        [(? symbol?)                (id sexp)]
        [(list 'fun (list p) b)                 (fun p (parse b))]
        [(list f a)                 (app (parse f) (parse a))]
        [else                       (error 'parse "bad syntax: ~a" sexp)]))


;(parse '{1 2})
;(parse '{with {x 3} {+ x y}}) ; {{fun {x} {+ x y}} 3}
;(parse '{{fun {a} {+ 3 a}} 10})
;(parse '{+ 2 {fun {a} {+ 3 a}}})
;(test (parse '{{fun {x} {+ x 1}} 10})
;                   (app (fun 'x (add (id 'x) (num 1))) (num 10)))

; Type definition for deferred substitution
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value LFAE-Value?) (ds DefrdSub?)])

; Type Defintion for LFAE-Value
(define-type LFAE-Value
  [numV       (n number?)]
  [closureV   (param symbol?) (body LFAE?) (ds DefrdSub?)]
  [exprV      (expr LFAE?) (ds DefrdSub?) (value (vector/c (or/c false LFAE-Value?)))]) ; Use a vector for caching


; strict: LFAE-Value -> LFAE-Value
; purpose: to interpret exprV expression to get a value in strictness points.
(define (strict v)
  (type-case LFAE-Value v
    [exprV (expr ds v-vec)
          (if (not (vector-ref v-vec 0))
              (local [(define v (strict (interp expr ds)))]
                       (vector-set! v-vec 0 v) v)
              (vector-ref v-vec 0))]
    [else v]))



; num-op: operators for arithmetic computation -> function for arithmetic computation
; purpose: to get a function for arithmetic computation.
(define (num-op op)
     (lambda (x y)
          (numV (op (numV-n x) (numV-n y)))))

(define num+ (num-op +))
(define num- (num-op -))

; lookup: symbol DefrdSub -> FAE-Value
; purpose: to get a value for the given identifier (symbol)
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub ()           (error 'lookup "free identifier")]
    [aSub  (i v saved) (if(symbol=? i name)
                                (strict v)             ;; if v is exprV (num ==> interp it
                                (lookup name saved))]))

; interp: LFAE DefrdSub -> LFAE-Value
; purpose: to get LFAE-Value from LFAE
(define (interp lfae ds)
  (type-case LFAE lfae
     [num (n)      (numV n)]
     [add (l r)    (num+ (interp l ds) (interp r ds))]
     [sub (l r)    (num- (interp l ds) (interp r ds))]
     [id  (s)     (lookup s ds)]
     [fun (p b)  (closureV p b ds)]
     [app (f a)   (local [(define f-val (strict (interp f ds)))
                          (define a-val (exprV a ds (vector #f)))]
                   (interp (closureV-body f-val)
                           (aSub (closureV-param f-val)
                                 a-val
                                 (closureV-ds f-val))))]))

; run: sexp -> LFAE-Value
; purpose: to run parse and interp in one queue.
(define (run sexp ds)
     (interp (parse sexp) ds))

;(run '{with {y 3} {{fun {x} {+ x y}} {+ 24 9 }}} (mtSub))

(run '{with {x 3}
{with {f {fun {y} {+ x y}}}
{with {x 5}
{f 4}}}} (mtSub)) ;should be 7 in static

(parse '{with {y 3} {{fun {x} {+ x y}} {+ 24 9 }}})

(run '{with {y 3} {{fun {x} {+ x y}} {+ 24 9 }}} (mtSub))

(run '{with {z 3} {with {z 8} {{fun {x} {- z x}} 2}}} (mtSub))

(interp (parse '{with {x 3} {with {f {fun {y} {+ x y}}} {with {x 5} {f 4}}}}) (mtSub))

;(run '{fun {y} {+ x y}} (mtSub))
;(run '{{fun {x} {+ 1 x}} 10} (mtSub))
;(run '{{fun {x} 0} {+ 1 {fun {y} 2}}} (mtSub))
;(run '{+ 1 {fun {y} 2}} (mtSub))
;(run '{{fun {x} x} {+ 1 {fun {y} 2}}} (mtSub))
;(run '{{fun {x} x} {+ 1 {{fun {y} 2} 1}}} (mtSub))
;(run '{{fun {x} x} {+ 1 1}} (mtSub))

;(run '{{fun {x} x} 1} (mtSub))
;(run '{{fun {x} {+ x x}} 1} (mtSub))
;(run '{{fun {x} x} {+ 1 {fun {y} 2}}} (mtSub))
;(run '{{fun {x} {+ x x}} {+ 1 {fun {y} 2}}} (mtSub))
;(run '{{fun {x} {+ x x}} {+ 1 {{fun {y} 2} 1}}} (mtSub))
;(run '{{fun {x} {+ x x}} 1} (mtSub))
;(run '{{fun {x} {+ {+ x x} {+ x x}}} {- {+ 4 5} {+ 8 9}}} {mtSub})

;(run '{{fun {x} {x {+ 4 5}}} {fun {x} 0}} (mtSub)) 
;(run '{{fun {x} 0} {+ 1 {fun {y} 2}}} (mtSub)) 
;(run '{{fun {x} {+ x x}} {+ 1 {{fun {y} 2} 1}}} (mtSub))
;(run '{{fun {x} x} 1} (mtSub))
;(run '{{fun {x} x} {+ 1 {{fun {y} 2} 1}}} (mtSub))

;(run '{{fun {f} {f 1}} {fun {x} {+ x 1}}} (mtSub))
;(run '{{fun {x} {+ x x}} {+ 1 {{fun {y} 2} 1}}} (mtSub))
;(run '{with {a {fun {f} {+ f f}}} {a 3}} (mtSub))

;(parse '{{fun {f} {f 1}} {fun {x} {+ x 1}} })

;(interp (parse '{fun {f} {f 1}}) (mtSub))

;(interp (app (id 'f) (num 1)) (aSub 'f (exprV (fun 'x (add (id 'x) (num 1))) (mtSub) (vector #f)) (mtSub))) ; first execution

;(lookup 'f (aSub 'f (exprV (fun 'x (add (id 'x) (num 1))) (mtSub) (vector #f)) (mtSub)))

;(strict (lookup 'f (aSub 'f (exprV (fun 'x (add (id 'x) (num 1))) (mtSub) (vector #f)) (mtSub)))) ; second loop f-value

;(interp (add (id 'x) (num 1)) (aSub 'x (exprV (num 1) (mtSub) (vector #f)) (mtSub))) ; how?

;current ds: (aSub 'f (exprV (fun 'x (add (id 'x) (num 1))) (mtSub) (vector #f)) (mtSub))
;a-val: (aSub 'x (exprV (num 1) (aSub 'x (exprV (num 1) (mtSub) (vector #f))) (vector #f) (mtSub)) 
;f-val (closureV 'f (app (id 'f) (num 1)) (mtSub))

;(interp (add (id 'x) (num 1)) (aSub 'x (exprV (num 1) (aSub 'f (exprV (fun 'x (add (id 'x) (num 1))) (mtSub) (vector #f)) (mtSub)) (vector #f)) (mtSub))) ;interp for second one

;(lookup 'x (aSub 'x (exprV (num 1) (aSub 'f (exprV (fun 'x (add (id 'x) (num 1))) (mtSub) (vector #f)) (mtSub)) (vector #f)) (mtSub)))

;(run '{{fun {x} x} {+ 1 {fun {y} 2}}} (mtSub)) ;; numV-n: contract violation ... [The interp is terminated with an error]

