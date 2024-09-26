#lang plai

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)])


(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'with (list i v) e) (with i (parse v)(parse e))]
    [(? symbol?) (id sexp)]
    [else (error 'parse "bad syntax:~a" sexp)]
    ))


(define (subst wae idtf val)
	(type-case WAE wae
		[num (n) wae]
		[add (l r) (add (subst l idtf val) (subst r idtf val))]
		[sub (l r)(sub (subst l idtf val)(subst r idtf val))]
		[with (i v e) (with i (subst v idtf val) (if (symbol=? i idtf) e
									(subst e idtf val)))]
		[id (s) (if (symbol=? s idtf) (num val) wae)]))

(define (interp wae)
	(type-case WAE wae
		[num (n) n]
		[add (l r) (+ (interp l) (interp r))]
		[sub (l r) (- (interp l) (interp r))]
		[with (i v e) (interp (subst e i (interp v)))]
		[id (s)		(error 'interp "free identifier")]))

(test (interp (with 'x (num 5) (add (id 'x) (id 'x)))) 10)

(test (subst (sub (id 'x) (num 1)) 'y 10)    (sub (id 'x) (num 1)))

(test (subst (with 'x (id 'y) (id 'x)) 'x 10) (with 'x (id 'y) (id 'x)))

(parse '(+ 1 2 ))
