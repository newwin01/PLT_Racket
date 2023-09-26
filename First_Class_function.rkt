#lang plai

;; FWAE definition

(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?)(rhs FWAE?)]
  [sub (lhs FWAE?)(rhs FWAE?)]
  [with (name symbol?)(named-expr FWAE?)(body FWAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FWAE?)]
  [app (ftn FWAE?)(arg FWAE?)])

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l)(parse r))]
    [(list '- l r) (sub (parse l)(parse r))]
    [(list 'with (list i v) e)  (with  i (parse v) (parse e))]
    [(? symbol?) (id sexp)]
    [(list 'fun (list p) b) (fun p (parse b))]
    [(list f a) (app (parse f) (parse a))]
    [else (error 'parse "bad syntax: ~a" sexp)]))

(test (parse '{{fun {x} {+ x 1}} 10})
                    (app (fun 'x (add (id 'x) (num 1))) (num 10)))


(define (num-op op) (lambda (x y) (num (op (num-n x) (num-n y))))) ;;actually reutrn function itself, return function itself, contract will be the function

;; (define (num+ x y) (num (+ (num-n x) (num-n y))))
;; (define (num- x y) (num (- (num-n y) (num-n y))))

(define num+ (num-op +))
(define num- (num-op -))

(num+ (num 3) (num 5))


(define (interp fwae)
 (type-case FWAE fwae
  [num  (n)      fwae]
  [add  (l r)      (num+ (interp l) (interp r))]
  [sub  (l r)      (num- (interp l) (interp r))]
  [with (i v e)  (interp (subst e i (interp v)))]
  [id     (s)       (error 'interp "free identifer")]
  [fun (p b) fwae]
  [app  (f a)    (local [(define ftn (interp f))]
                   (interp (subst (fun-body ftn) (fun-param ftn) (interp a))))]))


(define (subst exp idtf val)
	(type-case FWAE exp
		[num (n) exp]
		[add (l r) (add (subst l idtf val) (subst r idtf val))]
		[sub (l r) (sub (subst l idtf val) (subst r idtf val))]
		[with (i v e) (with i (subst v idtf val) (if (symbol=? i idtf) e
                                                             (subst e idtf val)))]
		[id (s) (if (symbol=? s idtf) val exp)]
		[app (f a)(app f	(subst f idtf val) (subst a idtf val))]
          [fun (id body) (if (equal? idtf id) exp (fun id (subst body idtf val)))]
          ))

;(test (interp (with 'x (num 5) (add (id 'x) (id 'x)))) (num 10))

;(test (interp (parse '(fun {a} {+ a a}))) (fun 'a (add (id 'a) (id 'a))))

(test (interp (parse '{with {fn {fun {a} {+ a a}}} {with {x 1} {fn {with {y 10} {+ y x}}}}})) (num 22))