#lang racket
(require racket/trace)

; Function recursively counts the number of items in a list
; EDIT: Now tail-recursive via an accumulator variable
(define (listLength myList [acc 0])
  (if (empty? myList)    ; Check if list empty
      acc
      (listLength (cdr myList) (+ 1 acc))) )    ; Count rest of elements in list

; Function recursively counts the number of atoms in a list
; EDIT: Now tail-recursive via a helper function
(define (deepListLength myList)
  (define (helper currList acc)
    (cond
      [(empty? currList) acc]    ; List is empty
      [(list? (car currList))    ; Head of list is a sublist
       (helper (cdr currList) (helper (car currList) acc))]    ; Recursively count atoms in sublist and remaining elements
      [else
       (helper (cdr currList) (+ acc 1))]))    ; Increment total when atom is found
  (helper myList 0))    ; Recursively traverse the list


; Define two lists for testing
(define testListInt '(1 2 3 4))    ; List contains only integers
(define testListAtom '(1 (2 3) (4 (5 6))))    ; List contains integers and lists

; Display lists
(printf "** Lists **\n")
(printf "First list: ~v\n" testListInt)
(printf "Second list: ~v\n" testListAtom)

; Test first function on both lists
(printf "\n** List Length Function **\n")
(printf "Number of items in the first list: ~v\n" (listLength testListInt))
(printf "Number of items in the second list: ~v\n" (listLength testListAtom))

; Test second function on both lists
(printf "\n** Deep List Length Function **\n")
(printf "Number of atoms in the first list: ~v\n" (deepListLength testListInt))
(printf "Number of atoms in the second list: ~v\n" (deepListLength testListAtom))