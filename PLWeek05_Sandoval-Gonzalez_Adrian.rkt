#lang racket

; Function defines the first polynomial 9x^2-3x-25
(define (myPolynomial x) (- (- (* 9 (* x x )) (* 3 x)) 25))

; Function defines the second polynomial 11x^2-2x-50
(define (otherPolynomial x) (- (- (* 11 (* x x )) (* 2 x)) 50))

; Function finds the root of a polynomial using Steffensen's method and recursion
(define (findRoot polynomial x0 tolerance)
  (define y0 (polynomial x0))    ; Get current y
  
  (if (< (abs y0) tolerance)    ; Check if absval of current y is less than tolerance
      (printf "Root x-value: ~v \nFinal y-value: ~s" x0 y0)    ; Display final values
      ;(printf "Final y-value: ~" y0)
      (let ((x1 (- x0 (/ (expt y0 2) (- (polynomial (+ x0 y0)) y0)))))    ; Steffensen's method
        (findRoot polynomial x1 tolerance))))    ; Recursively call findRoot with new approximation

; Test first function
(printf "First Function: y = 9x^2 - 3x - 25\n")
(define accuracy 0.00001)

(let ([x0 4.0])
(findRoot myPolynomial x0 accuracy))

; Test second function
(printf "\n\nSecond Function: y = 11x^2 - 2x - 50\n")

(let ([x0 1.0])
(findRoot otherPolynomial x0 accuracy))