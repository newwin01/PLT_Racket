#lang plai

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value number?)
        (saved DefrdSub?)])


(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)(rhs F1WAE?)]
  [sub (lhs F1WAE?)(rhs F1WAE?)]
  [with (name symbol?)(named-expr F1WAE?)(body F1WAE?)]
  [id (name symbol?)]
  [app (ftn symbol?)(arg F1WAE?)])


(define (lookup-fundef name fundefs)
	(cond
          [(empty? fundefs)
           (error 'lookup-fundef "unknown function")]
          [else
           (if (symbol=? name (fundef-fun-name (first fundefs)))
               (first fundefs)
               (lookup-fundef name (rest fundefs)))]))

(define (parse-fd sexp)
  (match sexp
    [(list 'deffun (list f x) b) (fundef f x (parse b))]))

(define (subst f1wae idtf val)
	(type-case F1WAE f1wae
		[num (n) f1wae]
		[add (l r) (add (subst l idtf val) (subst r idtf val))]
		[sub (l r) (sub (subst l idtf val) (subst r idtf val))]
		[with (i v e) (with i (subst v idtf val) (if (symbol=? i idtf) e
                                                             (subst e idtf val)))]
		[id (s) (if (symbol=? s idtf) (num val) f1wae)]
		[app (f a)(app f	(subst a idtf val))]))


(define (lookup name ds)
      (type-case DefrdSub ds
            [mtSub       ()                  (error 'lookup "free identifier")]
            [aSub      (i v saved)      (if (symbol=? i name)
                                                                     v
                                                                     (lookup name saved))]))

(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l)(parse r))]
    [(list '- l r) (add (parse l)(parse r))]
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [(? symbol?) (id sexp)]
    [(list f a) (app f (parse a))]
    [else (error 'parse "bad syntax: ~a" sexp)]))

(define (interp f1wae fundefs ds)
 (type-case F1WAE f1wae
  [num  (n)      n]
  [add  (l r)      (+ (interp l fundefs ds) (interp r fundefs ds))]
  [sub  (l r)      (- (interp l fundefs ds) (interp r fundefs ds))]
  [with (i v e)  (interp e fundefs (aSub i (interp v fundefs ds) ds))]
  [id     (s)       (lookup s ds)]
  [app  (f a)    (local
 		                   [(define a-fundef (lookup-fundef f fundefs))]
		                   (interp (fundef-body a-fundef)
	                                         fundefs
                                                 (aSub (fundef-arg-name a-fundef)
	                                                    (interp a fundefs ds)
		                                            (mtSub))  
                                     ))]))

(test (interp (parse '{f 1}) (list (parse-fd '{deffun (f x) {+ x 3}})) (mtSub)) 4) 
