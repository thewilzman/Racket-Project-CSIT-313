#lang racket

;; Problem 1

;;----------------------------
;;
;; function delall
;;
;; input:
;; v an atom
;; myList a list
;;
;; returns myList with all occurences of v removed
;; including where it was embedded in sublists
;;
;;-----------------------------
(define (delall v myList)
  (cond
    ((empty? myList) myList)
    ((equal? v (car myList)) (delall v (cdr myList)))
    ((list? (car myList)) (cons (delall v (car myList)) (delall v (cdr myList))))   
    (else (cons (car myList) (delall v (cdr myList))))
    )
 )


;; Problem 2

;;----------------------------
;;
;; function getextremes
;;
;; input:
;; myList a list of numbers
;;
;; returns a tuple formed of the largest and smallest list values
;;
;;-----------------------------
(define (getextremes myList)
  (cond
    ((equal? (length myList) 1) (list (car myList) (car myList)))
    ((empty? myList) '())
    (else (list (cond
                  ((> (car myList) (car (getextremes (cdr myList)))) (car myList))
                  (else (car (getextremes (cdr myList))))
                  )
                (cond
                  ((< (car myList) (cadr (getextremes (cdr myList)))) (car myList))
                  (else (cadr (getextremes (cdr myList))))
                  )
           )
     )
    )
 )


;; Problem 3

;;----------------------------
;;
;; function findval?
;;
;; input:
;; v an atom
;; myList a list
;;
;; searches for v in myList and returns #t if found, #f otherwise
;;
;;-----------------------------
(define (findval? v myList)
  (cond
    ((empty? myList) #f)
    ((equal? v (car myList)) #t)
    (else (findval? v (cdr myList)))
    )
 )

;;----------------------------
;;
;; function unite
;;
;; input:
;; list1 a list (set)
;; list2 a list (set)
;;
;; returns a list that is the union of list1 and list2
;; (does not check if the lists are sets)
;;
;;-----------------------------
(define (unite list1 list2)
  (cond
    ((empty? list1) list2)
    ((findval? (car list1) list2) (unite (cdr list1) list2))
    (else (cons (car list1) (unite (cdr list1) list2)))
    )
  )
   


;;----------------------------
;;
;; function checkset
;;
;; input:
;; myList a list (set)
;;
;; checks if myList is a set (no duplicates),returns #t if yes, #f otherwise
;;
;;-----------------------------
(define (checkset myList)
  (cond
    ((empty? myList) #t)
    ((findval? (car myList) (cdr myList)) #f)
    (else (checkset (cdr myList)))
    )
  )
   


(provide (all-defined-out))

