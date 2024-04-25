#lang racket

;write a function  named getextremes to get the largest and the smallest number in a list
(define (getextremes l a b ;l is the list , a is the largest number and b is the smallest
         (cond
           ((not (list? l)) #f)
           ((empty? l) #f)
           (eq? (car l) (cdr l) #f)
           ((> (car l) a) (cdr l) a)
           ((and (< (car l) b) (cdr  l) b))
           (else (getextremes a b (cdr l))) 
         ) 
  )
  )



;write a function named unite that returns the union of two simple list parameters that represent sets
            
(define (unite a b ; a and b are considered a list in this function
         (cond
           ((not (list? a)) #f)
           ((not (list? b)) #t)
           ((empty? a) #f)
           ((empty? b) #t)
           ((member (car a) b) (unite (cdr a) b))
           (else (cons (car a) (unite (cdr a) b))
           )
         )
  )
 )
                               
           


;write a function named checkset that takes simple list of parameters and returns true if the list is a proper set
(define (checkset lst 
         (cond
           (not (list? lst) #f)
           ((empty? lst) #f)
           (eq? (car lst) (second lst) #f)
           (else (checkset (cdr lst)
         )
  )
 )
      )
  )
        
         


;;write a function named delall

(define (delall a lst) ;a is an atom and lst is a list 
  (cond
    ((empty? lst) '())
    ((list? (car lst))
     (cons (delall a (car lst))
           (delall a (cdr lst))))
  ((eq? (car lst) a) (delall a (cdr lst))))
  (else (cons (car lst) (delall a (cdr lst)))
        )
  )
  
    