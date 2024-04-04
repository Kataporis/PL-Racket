#lang racket

; Square function
(define (square num) (* num num) )

; This function evaluates the expression at a point
(define (firstFunction x)
  [ - (+ 30 (* 2 x)) (* 3 (square x)) ])

; This function evaluates the expression at a point and displays different output depending on the result
(define (secondFunction x)
  (define result [ - (+ 30 (* 2 x)) (* 3 (square x)) ] )
  (cond
    [(> result 10) (printf "The result is greater than 10. The function evaluated at x = ~v" x) (printf " is equal to: ~v\n"result) ]
    [(< result -10) (printf "The result is less than -10. The function evaluated at x = ~v" x) (printf " is equal to: ~v\n"result) ]
    [else (printf "The result is not greater than 10 or less than -10. The function evaluated at x = ~v" x) (printf " is equal to: ~v\n"result) ] ) )

; Test first function
(printf "**First Function**\n")
(define testOne 2)
(printf "The function evaluated at x = ~v" testOne)
(printf " is equal to: ~v\n"(firstFunction 2))

; Test second function for a result greater than 10
(printf "\n**Second Function**\n")
(secondFunction testOne)

; Test second function for a result less than -10
(define testTwo -10)
(secondFunction testTwo)

; Test second function for a result not greater than 10 or less than -10
(define testThree -3)
(secondFunction testThree)