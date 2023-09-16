#lang plai

;Problem 6

; subproblem a

(define-type Vehicle
  (Bycicle (wheels number?))
  (Car (wheels number?)
       (windows number?))
  (Airplane (wheels number?)
            (windows number?)
            (engines number?))
  )

(define myCar(Car 2 2))
(define myPlane(Airplane 5 11 6))

; subproblem b
(define (vehicle-tax v tax_wh tax_wd tax_e)
  (type-case Vehicle v
  [Bycicle (wh) (* tax_wh wh)]
  [Car (wh wd) (+ (* tax_wh wh) (* tax_wd wd))]
  [Airplane (wh wd e) (+ (* tax_wh wh) (+(* tax_wd wd)) (* tax_e e))]
    )
 )


(test (vehicle-tax myCar 10 20 30) 60)
(test (vehicle-tax myPlane 1 5 10) 120)

; subproblem c

(define (safe-or-not bool)
  (cond
    [bool "safe"]
    [else "unsafe"]
    )
  )

(define (is-vehicle-safe v)
(type-case Vehicle v
  [Bycicle (wh) ( safe-or-not (< wh 4)) ]
  [Car (wh wd) (safe-or-not (and ( > wh 3) ( > wd 2 ))) ]
  [Airplane (wh wd e) (safe-or-not (and (> wh 2) (and ( > wd 10)) ( > e 1) ) )]
  )
)

(test (is-vehicle-safe myCar) "unsafe")
(test (is-vehicle-safe myPlane) "safe")

;Problem 7

(define (alpha-to-word alpha)
  (cond
    [(symbol=? alpha 'a) 'alice]
    [(symbol=? alpha 'c) 'cherry]
    [(symbol=? alpha 'j) 'jc]
    [(symbol=? alpha 'k) 'kate]
    [else 'unnamed]
))

(test (alpha-to-word 'a) 'alice)
(test (alpha-to-word 'c) 'cherry)
(test (alpha-to-word 'q) 'unnamed)

(define (name-alphabet l_alphabets)
 (cond
   [(empty? l_alphabets) empty]
   [else (cons (alpha-to-word (first l_alphabets)) (name-alphabet (rest l_alphabets)))] 
   ))

(test (name-alphabet '(a b n)) '(alice unnamed unnamed) )
(test (name-alphabet '(j c k)) '(jc cherry kate) )
(test (name-alphabet '()) '() )

;Problem 8

(define (update-name old_sym new_sym l_sym)
   (cond
     [(empty? l_sym) empty]
     [else (cond
             [(symbol=? (first l_sym) old_sym) (cons new_sym (update-name old_sym new_sym (rest l_sym)))]
             [else (cons (first l_sym) (update-name old_sym new_sym (rest l_sym)))]
             )
           ]
     )
)

(test (update-name 'cherry 'claire (cons 'cherry empty)) '(claire))