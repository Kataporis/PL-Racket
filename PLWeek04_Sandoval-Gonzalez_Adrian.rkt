#lang racket

; Function calculates the value at a term for the power series and calls another function to determine the sign
(define (seriesTerm terms denom)
  ( * (findSign terms) [ expt (/ 1 denom) (- terms 1) ] ) )

; Function calculates the value at a term for the power series and uses a lambda function to determine the sign
(define (seriesTermLambda terms denom)
  ( * ((lambda (i)
         (if (= 0 (modulo i 2) ) -1 1)) terms)
         [ expt (/ 1 denom) (- terms 1) ] ) )

; Function determines whether to multiply by 1 or -1
(define (findSign i)
  (if (= 0 (modulo i 2) ) -1 1) )

; Function recursively calculates a power series to a number of terms
(define (powerSeries terms denom)
  (if (< terms 2)
      1
      (+ (seriesTerm terms denom) (powerSeries (- terms 1) denom) ) ) )

; Function recursively calculates a power series to a number of terms using a lambda function
(define (powerSeriesUsesLambda terms denom)
  (if (< terms 2)
      1
      (+ (seriesTermLambda terms denom) (powerSeries (- terms 1) denom) ) ) )

; Approximate the power series to a number of terms with a denominator of 3 and display
(define numTerms 20)
(define denominatorOne 3)
(printf "** Named Function **\n")
(printf "The power series calculated to ~v" numTerms)
(printf " terms with an n value of ~v" denominatorOne)
(printf " converges to: ~v\n"(exact->inexact[powerSeries numTerms denominatorOne]))

; Approximate again with a denominator of 4
(define denominatorTwo 4)
(printf "The power series calculated to ~v" numTerms)
(printf " terms with an n value of ~v" denominatorTwo)
(printf " converges to: ~v\n"(exact->inexact[powerSeries numTerms denominatorTwo]))

; Re-do denominator of 3 using lambda function
(printf "\n** Lambda Function **\n")
(printf "The power series calculated to ~v" numTerms)
(printf " terms with an n value of ~v" denominatorOne)
(printf " converges to: ~v\n"(exact->inexact[powerSeriesUsesLambda numTerms denominatorOne]))

; Re-do denominator of 4 using lambda function
(printf "The power series calculated to ~v" numTerms)
(printf " terms with an n value of ~v" denominatorTwo)
(printf " converges to: ~v\n"(exact->inexact[powerSeriesUsesLambda numTerms denominatorTwo]))