#lang plai

;Problem 1

(define (dollar->won dollar) ( * 1324 dollar))

(test (dollar->won 1) 1324)
(test (dollar->won 0.5) 662)

;Problem 2

(define (max num1 num2)
  (cond
    [(<= num1 num2) num2]
    [else num1]))

(test (max 4 5) 5)
(test (max 6 5) 6)

(define (max-of-three-integers num1 num2 num3)
  (max num1 (max num2 num3)))

(test (max-of-three-integers 3 2 1) 3)
(test (max-of-three-integers 1 2 3) 3)
(test (max-of-three-integers 3 3 3) 3)

;Problem 3

(define (volume-cuboid length breadth height)
  (* length (* breadth height )))

(test (volume-cuboid 1 2 3) 6)
(test (volume-cuboid 5 6 10) 300)

;Problem 4

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

(define (factorial n)
  (cond
    [(= n 0) 1]
    [else (* n (factorial (- n 1)))]
  ))

(test (factorial 10) 3628800)


(define (combination n k)
  (/ (factorial n) ( * (factorial k) (factorial (- n k ))))
  )

(test (combination 10 1) 10)
(test (combination 30 11) 54627300)
