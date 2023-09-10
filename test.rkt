#lang plai

(define-type AE
  [num (n number?)]
  [add (lhs AE?)
                    (rhs AE?)]
  [sub (lhs AE?)
                    (rhs AE?)]
)

(define (parse sexp)
   (cond
     [(number? sexp) (num sexp)]
     [(eq? (second sexp) '+) (add (parse (first sexp)) (parse (third sexp)))]
     [(eq? (second sexp) '-) (sub (parse (first sexp)) (parse (third sexp)))]
))

(parse '{3 + (5 + 4)})

(define (interp an-ae)
 (type-case AE an-ae
     [num (n) n]
     [add (l r) (+ (interp l) (interp r))]
  [sub (l r) (+ (interp l) (interp r))]))

(interp (parse '{3 + (5 + 4)}))