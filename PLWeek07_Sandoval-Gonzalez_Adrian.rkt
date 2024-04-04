#lang racket

; Function finds the middle value of first three numbers in the list
(define (middleOfThree lst)
  (cond
    ; Check if list has fewer than three elements
    ((< (length lst) 3) (first lst))    ; Rerturn first element if so
    
    ; List has three or more elements
    (else
      (let ([firstElem (first lst)]    ; Get first element
            [secondElem (second lst)]  ; Get second element
            [thirdElem (third lst)])   ; Get third element
        (cond
          ; Check if first element is middle val
          ((and (<= firstElem secondElem) (>= firstElem thirdElem)) firstElem)
          ((and (>= firstElem secondElem) (<= firstElem thirdElem)) firstElem)

          ; Check if second element is middle val
          ((and (<= secondElem firstElem) (>= secondElem thirdElem)) secondElem)
          ((and (>= secondElem firstElem) (<= secondElem thirdElem)) secondElem)

          ; Else the third element is middle val
          (else thirdElem))))))

(define (myQuicksort lst)
  (if (or (empty? lst) (empty? (rest lst)))    ; If the list is empty or has one element then already sorted
      lst
      (let* ((pivot (middleOfThree lst))    ; Use middleOfThree if length of list is at least 3. Otherwise, first element is pivot
             (lessThanList (filter (lambda (x) (< x pivot)) lst))      ; Filter all values less than pivot
             (equalToList (filter (lambda (x) (= x pivot)) lst))       ; Filter all values equal to pivot
             (greaterThanList (filter (lambda (x) (> x pivot)) lst)))  ; Filter all values greater than pivot
        (append (myQuicksort lessThanList) equalToList (myQuicksort greaterThanList)))))    ; Combine the lists

; Test the myQuicksort function
(define testList '(20 13 74 5 12 9 22 94 22 6 96 72 3 53 33 21 101 3 17 15 95 88))
(printf "** List Being Tested on the myQuickSort Function **\n")
(printf "List: ~v\n" testList)

(printf "\n** myQuickSort Function Results **\n")
(printf "Sorted list: ~v" (myQuicksort testList))