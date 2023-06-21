#| 
Author: Pradyoth Velagapudi
Date: 9 February 2023
Description: This program is made for Scheme Assignment 1 in Prof. Selgrad's CSE240 class to demonstrate Scheme fundamentals.
|#

#lang r5rs 

;Part 1 - Warming up to Scheme with Some Ridiculous Math
;(these functions evaluate arithmetic expressions)

;4 + 5 + 6 = 15
(define run1(lambda () (+ 4 (+ 5 6))) ) 

;5 * (4 + 5) / 2 = 22 1/2
(define run2(lambda () (/ (* 5 (+ 4 5)) 2) ) ) 

;100 – ((20/5)*(3*3)) + (120/10) = 76
(define run3(lambda ()
              (+ (- 100 (* (/ 20 5) (* 3 3)))
                    (/ 120 10) ) ) )

;5 * ( 4 + ( ( ( 10 + 10 ) + ( 5 * 8 ) ) / ( 10 + 2 ) ) ) = 45
(define run4(lambda ()
              (* 5 (+ 4
                    (/ (+ (+ 10 10) (* 5 8)) (+ 10 2))))  ) )

;( ( ( ( ( ( 3 + 5 ) * ( 6 + 4 ) ) / 2 ) / 2 ) – 5 ) / 3) + ( ( ( ( 2 * 10 ) + ( 5 * 4 ) ) / 2 ) + ( 4 * 5 ) ) = 45
(define run5(lambda () (+
                        (/ (- (/ (/ (* (+ 3 5) (+ 6 4)) 2) 2) 5) 3)
                        (+ (/ (+ (* 2 10) (* 5 4)) 2) (* 4 5))  ) ) )

;--------------------------------------------------------------------------------------------------------------------------

;Part 2 – Visiting Final Fantasy
;(these functions calculate damage in the rpg Final Fantasy

;simple damage calculation from FF6
(define ff6-Damage(lambda (damage defense) (floor (+ (/ ( * damage (- 255 defense)) 256) 1)) ) )

;complex damage calculation from FFX, takes defense into account
(define ffX-baseDamage(lambda (stat dmgValue) (* (floor (+ (/ (expt stat 3) 32) 32)) (floor (/ dmgValue 16))) ) )
(define ffX-defenseNum(lambda (defense) (+ (floor (/ (expt (- defense 280.4) 2) 110)) 16) ) )
(define ffX-reduceDamage(lambda (baseDmg defenseNum) (floor (/ (* baseDmg defenseNum) 730)) ) )
(define ffX-finalDamage(lambda (ffX-reduceDamage defense) (floor (/ (* ffX-reduceDamage (- 730 (floor (/ (- (* defense 51) (/ (expt defense 2) 11) ) 10)))) 730)) ) )

;uses previous FFX functions to give final damage calculation for FFX
(define ffX-calculate-damage(lambda (stat dmgValue defense) (ffX-finalDamage (ffX-reduceDamage (ffX-baseDamage stat dmgValue) (ffX-defenseNum defense)) defense) ) )

;--------------------------------------------------------------------------------------------------------------------------

;Part 3 – List Manipulation
;(these functions demonstrate list manipulation techniques)

;return the second item of a list
(define get-second-item(lambda (list) (car (cdr list)))) ;car of the cdr-- same as cadr

;return the third item of a list
(define get-third-item(lambda (list) (car (cdr (cdr list))) ) ) ;car of the cdr of the cdr-- same as caddr

;return the length of a list
(define list-length?(lambda(list) (
                                   cond
                                     ((null? list) 0) ;base case: list is empty, return 0
                                     (else (+ 1 (list-length? (cdr list)))) ;recursive case; add 1 to the result of list-length? on the cdr 
                                     )
                                   ))

;return the cdr after the nth item in a list
(define arbitrary-cdr(lambda (number list) (
                                            cond
                                             ((> number (list-length? list)) #f) ;check to see if number param is in bounds of list. if not return #f
                                             (else (
                                                    cond
                                                     ((= number 1) list) ;base case: number = 1, return what remains of the list
                                                     (else (arbitrary-cdr (- number 1) (cdr list)))) ) ;recursive case: decrement the number param by 1, run arbitrary-cdr on cdr of list
                                                   )
                                             ) )

;construct a list repeating a given value
(define make-list(lambda (value size) (
                                       cond
                                        ((not (number? size)) `()) ;check to see if size param is a number. If not, return empty list
                                        (else (
                                               cond
                                                ((<= size 0) `()) ;base case: size = 0, return empty list
                                                (else (append (list value) (make-list value (- size 1))) ) ;recursive case: append a list containing 1 item of given value, to the list produced by running make-list with size-1
                                                )
                                              )
                                        )))

;--------------------------------------------------------------------------------------------------------------------------

;Part 4 - Multiply Number List
;(these functions work together to build a function capable of returning the product of all the items in a given list

;helper function: determines if a given list is composed entirely of numbers. If yes, returns #t, if no, returns #n
(define number-list?(lambda (list) (
                                    cond
                                     ((null? list) #t) ;base case: list is empty, return #t
                                     (else (
                                            cond ;recursive case: check if the car is a number
                                             ((not (number? (car list))) #f) ;if not, return #f
                                             (else (number-list? (cdr list))) ;if yes, call number-list on the cdr
                                             )
                                           )
                                     )
                      ))

;helper function: blindly multiplies all the items in a given list, returns product
;my own function, not stipulated in assignment specification
(define list-product(lambda (list) (
                                    cond
                                     ((null? list) 1) ;base case: list is empty, return 1
                                     (else (* (car list) (list-product (cdr list)))) ;recursive case: multiply the car with the result of list-product on the cdr
                                     )
                      ))

;final interface function: user uses this to multiply a number list. Calls both helper functions
(define multiply-number-list(lambda (list) (
                                            cond
                                             ((number-list? list) (list-product list)) ;check if the list is composed of entirely numbers. If so, return the product of all items
                                             (else #f) ;if list is not composed of entirely numbers, return #f
                                             )
                              ))
                                              