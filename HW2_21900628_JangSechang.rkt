#lang plai

;21900628 Jang Sechang
;Time taken: 3 hours


;[purpose] Type for the function definition, indicates function name, argument name and body which is F1WAE type
(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

;[purpose] F1WAE type which is the abstract syntax representation of Arithematic functions that support functions and substitutions
(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?)(rhs F1WAE?)]
  [sub (lhs F1WAE?)(rhs F1WAE?)]
  [with (name symbol?)(named-expr F1WAE?)(body F1WAE?)]
  [id (name symbol?)]
  [app (ftn symbol?)(arg F1WAE?)])

;[Contract] symbol list-of-FunDef -> error or Fundef
;[Purpose] look up list the function definitions and return the definition of functions of input function names recursively
;[test]
;(test (lookup-fundef 'identify (list (fundef 'identify 'x (id 'x)) (fundef 'twice 'x (add (id 'x) (id 'x))))) (fundef 'identify 'x (id 'x)))
;(test (lookup-fundef 'twice (list (fundef 'identify 'x (id 'x)) (fundef 'twice 'x (add (id 'x) (id 'x))))) (fundef 'twice 'x (add (id 'x) (id 'x))))
;(test/exn (lookup-fundef 'a (list (fundef 'identify 'x (id 'x)) (fundef 'twice 'x (add (id 'x) (id 'x))))) "unknown function")

(define (lookup-fundef name fundefs)
	(cond
          [(empty? fundefs)
           (error 'lookup-fundef "unknown function")]
          [else
           (if (symbol=? name (fundef-fun-name (first fundefs)))
               (first fundefs)
               (lookup-fundef name (rest fundefs)))]))
;[test]
(test (lookup-fundef 'identify (list (fundef 'identify 'x (id 'x)) (fundef 'twice 'x (add (id 'x) (id 'x))))) (fundef 'identify 'x (id 'x)))
(test (lookup-fundef 'twice (list (fundef 'identify 'x (id 'x)) (fundef 'twice 'x (add (id 'x) (id 'x))))) (fundef 'twice 'x (add (id 'x) (id 'x))))
;Error occuring test case
(test/exn (lookup-fundef 'a (list (fundef 'identify 'x (id 'x)) (fundef 'twice 'x (add (id 'x) (id 'x))))) "unknown function")

; [Contract] sexp -> F1WAE
; [Purpose] Receive the experssion written based on BNF and converted to Abstract syntax representation for intepreter
; [test]
;(test (parse '(+ 10 20)) (add (num 10) (num 20)))
;(test (parse '(- 10 20)) (sub (num 10) (num 20)))
;(test (parse '{with {x 1} {with {y 2} {+ y x}}}) (with 'x (num 1) (with 'y (num 2) (add (id 'y) (id 'x)))))
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(list '+ l r) (add (parse l)(parse r))]
    [(list '- l r) (sub (parse l)(parse r))]
    [(list '* l r) (desugar l r)]
    [(list 'with (list i v) e) (with i (parse v) (parse e))]
    [(? symbol?) (id sexp)]
    [(list f a) (app f (parse a))]
    [else (error 'parse "bad syntax: ~a" sexp)]))
; [test]
(test (parse '(+ 10 20)) (add (num 10) (num 20)))
(test (parse '(- 10 20)) (sub (num 10) (num 20)))
(test (parse '{with {x 1} {with {y 2} {+ y x}}}) (with 'x (num 1) (with 'y (num 2) (add (id 'y) (id 'x)))))

; [Contract] number number -> F1WAE
; [Purpose] Desugar sugared multiplication subexpression for interpreter
;[test]
;(test (parse '(* 1 2)) (add (num 1) (add (num 1) (num 0))))
;(test (parse '(* -6 3)) (add (num -6) (add (num -6) (add (num -6) (num 0)))))
;(test (parse '(* -7 -2)) (add (num 7) (add (num 7) (num 0))))
;(test (parse '(* 6 0)) (num 0))
(define (desugar multi count)
  (cond
    [(symbol? count) (error "Can't consume free identifer as multiplier")]
    [ (or (= count 0) (= multi 0)) (num 0)]
    [ (> count 0)
     (if (= count 1) (add (parse multi) (parse 0)) (add (parse multi) (desugar multi (- count 1))))]
    [ (< count 0)
      (if (number? multi)
          (if (= count -1) (add (parse (* multi -1)) (parse 0)) (add (parse (* multi -1)) (desugar multi (+ count 1))))
          (error "I can only negate number")
          )
       ]
    [else (error "desugar error")]
    ))

;[test]
(test (parse '(* 1 2)) (add (num 1) (add (num 1) (num 0))))
(test (parse '(* -6 3)) (add (num -6) (add (num -6) (add (num -6) (num 0)))))
(test (parse '(* -7 -2)) (add (num 7) (add (num 7) (num 0))))
(test (parse '(* 6 0)) (num 0))


;[Contract] sexp -> FunDef
;[Purpose] Translate deferring function definition of form of sexp with 'deffun keyword into real function definition, working with parser when sexp matches (list f a) form)
;[test]
;(test (parse-fd '{deffun (f x) {+ x 3}}) (fundef 'f 'x (add (id 'x) (num 3))))
;(test (list (parse-fd '(deffun (f x) {+ x 3})) (parse-fd '(deffun (f1 x) (- x 3)))) (list (fundef 'f 'x (add (id 'x) (num 3))) (fundef 'f1 'x (sub (id 'x) (num 3)))) )
(define (parse-fd sexp)
  (match sexp
    [(list 'deffun (list f x) b) (fundef f x (parse b))]))

;[test]
(test (parse-fd '{deffun (f x) {+ x 3}}) (fundef 'f 'x (add (id 'x) (num 3))))
(test (list (parse-fd '(deffun (f x) {+ x 3})) (parse-fd '(deffun (f1 x) (- x 3)))) (list (fundef 'f 'x (add (id 'x) (num 3))) (fundef 'f1 'x (sub (id 'x) (num 3)))) )

;[purpose] Type for Deferring substitution, storing the value that will be substituted
(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?)
        (value number?)
        (saved DefrdSub?)])

;[Contract] symbol DefrdSub -> number or error
;[Purpose] lookup deferring cache and find corresponding value for the cache name, used for interpreter
;[test]
;(test/exn (lookup 'a (mtSub)) "free identifier")
;(test (lookup 'a (aSub 'a 10 (mtSub))) 10)
;(test (lookup 'y (aSub 'x 1 (aSub 'y 4 (mtSub)))) 4)

(define (lookup name ds)
      (type-case DefrdSub ds
            [mtSub       ()                  (error 'lookup "free identifier")]
            [aSub      (i v saved)      (if (symbol=? i name) v (lookup name saved))]))
;[test]
;Error occuring test case
(test/exn (lookup 'a (mtSub)) "free identifier")
(test (lookup 'a (aSub 'a 10 (mtSub))) 10)
(test (lookup 'y (aSub 'x 1 (aSub 'y 4 (mtSub)))) 4)


;[Contract] F1WAE list-of-FunDef DefrdSub -> number
;[Purpose] Using Abstract syntax representation, list of defined function, and Deferring cache, calculate the results
;[test]
;(test (interp (add (num 10) (num 20)) (mtSub) (mtSub)) 30)
;(test (interp (parse '{f 1}) (list (parse-fd '{deffun (f x) {+ x 3}})) (mtSub)) 4)

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
;[test]
(test (interp (add (num 10) (num 20)) (mtSub) (mtSub)) 30)
(test (interp (parse '{f 1}) (list (parse-fd '{deffun (f x) {+ x 3}})) (mtSub)) 4)

;[test for syntax sugar]
(test (interp (parse '(* -6 3)) (mtSub) (mtSub))  -18)
(test (interp (parse '(* 6 3)) (mtSub) (mtSub))  18)
(test (interp (parse '(* -7 -3)) (mtSub) (mtSub))  21)
(test (interp (parse '(* 8 -3)) (mtSub) (mtSub))  -24)
(test (interp (parse '(* 1 0)) (mtSub) (mtSub)) 0)
(test (interp (parse '(* 1 1)) (mtSub) (mtSub)) 1)

; (1 Point) Q1. Which scope is supported for a free identifier in a function call in your implementation, static scope, dynamic scope or both?
; In my implementation, static scope is supported.

; (1 Point) Put the three test cases that show your answer for Q1 is correct. 

(test/exn (interp (parse '{with {y 2} {f 10}}) (list (parse-fd '{deffun (f x) {+ y x}})) (mtSub)) "free identifier") ;Interpreting function body starts with only one substitution
(test (interp (parse '{with {x 3} {f 10}}) (list (parse-fd '{deffun (f x) {+ 10 x}})) (mtSub)) 20) ;Showing it is using the x value 10 which is declared within the funtion arguments
(test/exn (interp (parse '{with {n 5} {f 10}}) (list (parse-fd '{deffun {f p} n})) (mtSub)) "free identifier") ;Showing the value n is free identifier within the function, not using the n value that is delcared on outer parameter