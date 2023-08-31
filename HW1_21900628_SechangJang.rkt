#lang plai

;Problem 1
;Time Taken: 1 min
;Solved by myself: Y
;[contract] dollar->won: number -> number
;[purpose] To convert dollar to won
;[test] (test (dollar->won 1) 1324)
;       (test (dollar->won 0.5) 662)

;Problem 1

(define (dollar->won dollar) ( * 1324 dollar))

(test (dollar->won 1) 1324)
(test (dollar->won 0.5) 662)

;Problem 2
;Time Taken: 3 min
;Solved by myself: Y

;[contract] max: number number -> number
;[purpose] Finding max number among the two integers
;[test] (test (max 4 5) 5)
;       (test (max 6 5) 6)

(define (max num1 num2)
  (cond
    [(<= num1 num2) num2]
    [else num1]))

(test (max 4 5) 5)
(test (max 6 5) 6)

;[contract] max-of-three-integers: number number number -> number
;[purpose] Finding max number among the three integers
;[test] (test (max-of-three-integers 3 2 1) 3)
;       (test (max-of-three-integers 1 2 3) 3)
;       (test (max-of-three-integers 3 3 3) 3)

(define (max-of-three-integers num1 num2 num3)
  (max num1 (max num2 num3)))

(test (max-of-three-integers 3 2 1) 3)
(test (max-of-three-integers 1 2 3) 3)
(test (max-of-three-integers 3 3 3) 3)

;Problem 3

;Time Taken: 2 min
;Solved by myself: Y

;[contract] volume-cuboid number number number -> number
;[purpose] Finding max number among the three integers
;[test] (test (volume-cuboid 1 2 3) 6)
;(test (volume-cuboid 5 6 10) 300)


(define (volume-cuboid length breadth height)
  (* length (* breadth height )))

(test (volume-cuboid 1 2 3) 6)
(test (volume-cuboid 5 6 10) 300)

;Problem 4

;Time Taken: 5 min
;Solved by myself: Y

;[contract] gcd number number -> number
;[purpose] Finding GCD between two numbers
;[test] (test (gcd 0 10) 10)
;(test (gcd 10 0) 10)
;(test (gcd 98 56) 14)
;(test (gcd 366 60) 6)
;(test (gcd 48 18) 6)

(define (gcd num1 num2)
  (cond
    [(= num2 0) num1]
    [else (gcd num2 (modulo num1 num2))]
    )
  )

(test (gcd 0 10) 10)
(test (gcd 10 0) 10)
(test (gcd 98 56) 14)
(test (gcd 366 60) 6)
(test (gcd 48 18) 6)

;Problem 5

;Time Taken: 7 min
;Solved by myself: Y

;[contract] factorial number -> number
;[purpose] Finding factorial of number
;[test] (test (factorial 10) 3628800)
; (test (factorial 5) 120)

(define (factorial n)
  (cond
    [(= n 0) 1]
    [else (* n (factorial (- n 1)))]
  ))

(test (factorial 10) 3628800)
(test (factorial 5) 120)

;[Contract] combination number number -> number
;[purpose] Finding combination of two numbers
;[test] (test (combination 10 1) 10)
;(test (combination 30 11) 54627300)

(define (combination n k)
  (/ (factorial n) ( * (factorial k) (factorial (- n k ))))
  )

(test (combination 10 1) 10)
(test (combination 30 11) 54627300)


;Problem 6

;Time Taken: 25 min
;Solved by myself: Y

;[contract] Vehicle
;[purpose] Define Type Vehicle with three variants Bycicle, Car, Airplane
;[test] (test (Car? myCar) #t)
;(test (Car? myPlane) #f)
;(test (Bycicle? myBicycle) #t)

; subproblem a

(define-type Vehicle
  (Bicycle (wheels number?))
  (Car (wheels number?)
       (windows number?))
  (Airplane (wheels number?)
            (windows number?)
            (engines number?))
  )

(define myCar(Car 2 2))
(test (Car? myCar) #t)
(define myPlane(Airplane 5 11 6))
(test (Car? myPlane) #f)
(define myBicycle(Bicycle 3))
(test (Bicycle? myBicycle) #t)

; subproblem b

;[contract] vehicle-tax Vehicle number number number -> number
;[purpose] Finding the tax of given Vehicle using input tax for wheel, window, and engine
;[test] (test (vehicle-tax myCar 10 20 30) 60)
;(test (vehicle-tax myBicycle 10 20 30) 30)
;(test (vehicle-tax myPlane 1 5 10) 120)

(define (vehicle-tax v tax_wh tax_wd tax_e)
  (type-case Vehicle v
  [Bicycle (wh) (* tax_wh wh)]
  [Car (wh wd) (+ (* tax_wh wh) (* tax_wd wd))]
  [Airplane (wh wd e) (+ (* tax_wh wh) (+(* tax_wd wd)) (* tax_e e))]
    )
 )


(test (vehicle-tax myCar 10 20 30) 60)
(test (vehicle-tax myBicycle 10 20 30) 30)
(test (vehicle-tax myPlane 1 5 10) 120)


; subproblem c

;[contract] safe-or-not bool -> string
;[purpose] Using the value of boolean, print whether it is safe or unsafe
;[test] (test (safe-or-not #t) "safe")
;(test (safe-or-not #f) "unsafe")

(define (safe-or-not bool)
  (cond
    [bool "safe"]
    [else "unsafe"]
    )
  )

(test (safe-or-not #t) "safe")
(test (safe-or-not #f) "unsafe")

;[contract] is-vehicle-safe Vehicle -> string
;[purpose] Decide whether the Vehicle is safe or not using the safety standard
;For a Bicycle, wheels must be less than 4. For a Car, wheels must be more than 3, windows must be more than 2.
;For an Airplane, wheels must be more than 2, windows must be more than 10, engines must be more than 1.
;(test (is-vehicle-safe myCar) "unsafe")
;(test (is-vehicle-safe myPlane) "safe")
;(test (is-vehicle-safe myBicycle) "safe")

(define (is-vehicle-safe v)
(type-case Vehicle v
  [Bicycle (wh) ( safe-or-not (< wh 4)) ]
  [Car (wh wd) (safe-or-not (and ( > wh 3) ( > wd 2 ))) ]
  [Airplane (wh wd e) (safe-or-not (and (> wh 2) (and ( > wd 10)) ( > e 1) ) )]
  )
)

(test (is-vehicle-safe myCar) "unsafe")
(test (is-vehicle-safe myPlane) "safe")
(test (is-vehicle-safe myBicycle) "safe")

;Problem 7

;Time Taken: 20 min
;Solved by myself: Y

;[contract] alpha-to-word symbol -> symbol
;[purpose] Convert specific alphabet to specific name
;[test] (test (alpha-to-word 'a) 'alice)
;(test (alpha-to-word 'c) 'cherry)
;(test (alpha-to-word 'q) 'unnamed)

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

;[contract] name-alphabet lists -> lists
;[purpose] Takes list of alphabets and change to list of names that has corresponding alphabet to names
;[test] (test (name-alphabet '(a b n)) '(alice unnamed unnamed) )
;(test (name-alphabet '(j c k)) '(jc cherry kate) )

(define (name-alphabet l_alphabets)
 (cond
   [(empty? l_alphabets) empty]
   [else (cons (alpha-to-word (first l_alphabets)) (name-alphabet (rest l_alphabets)))] 
   ))

(test (name-alphabet '(a b n)) '(alice unnamed unnamed) )
(test (name-alphabet '(j c k)) '(jc cherry kate) )

;Problem 8

;[contract] update-name symbol symbol lists -> lists
;[purpose] Takes old symbol and new symbol and substitute old symbol to new symbol in the given list
;[test] (test (update-name 'cherry 'claire (cons 'jc (cons 'cherry (cons 'kate empty)))) '(jc claire kate))

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

(test (update-name 'cherry 'claire (cons 'jc (cons 'cherry (cons 'kate empty)))) '(jc claire kate))
(test (update-name 'jc 'sc (cons 'jc (cons 'cherry (cons 'kate empty)))) '(sc cherry kate))