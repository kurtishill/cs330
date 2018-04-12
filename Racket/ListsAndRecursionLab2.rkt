#lang racket

(define (in-range start end value)
  (and (<= start value) (>= end value)
       true))

(define (check-temps1 temps)
  (if (empty? temps)
      true
      (and (in-range 5 95 (first temps))
           (check-temps1 (rest temps)))))

(define (check-temps temps low high)
  (if (empty? temps)
      true
      (and (in-range low high (first temps))
           (check-temps (rest temps) low high))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convert-aux digits count)
  (if (empty? digits)
      0
      (+ (* (first digits) count) (convert-aux (rest digits) (* 10 count)))))

(define (convert digits)
  (convert-aux digits 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (duple lst)
  (if (empty? lst)
      empty
      (cons (list (first lst) (first lst)) (duple (rest lst)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (average-aux lst)
  (if (empty? lst)
      0
      (+ (first lst) (average-aux (rest lst)))))

(define (average-aux2 lst)
  (if (empty? lst)
      0
      (+ 1 (average-aux2 (rest lst)))))

(define (average lst)
  (/ (average-aux lst) (average-aux2 lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (F-C temp)
  (* (- temp 32) (/ 5 9)))

(define (convertFC temps)
  (convertFC-aux temps empty))

(define (convertFC-aux temps lst)
  (if (empty? temps)
      empty
      (cons (F-C (first temps)) (convertFC-aux (rest temps) lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compare-values lst num)
  (if (> num (first lst))
      false
      (if (empty? (rest lst))
          true
          (compare-values (rest lst) num))))

(define (eliminate-larger-aux lst output)
  (if (empty? lst)
      output
      (if (compare-values lst (first lst))
          (cons (first lst) (eliminate-larger-aux (rest lst) output))
          (eliminate-larger-aux (rest lst) output))))

(define (eliminate-larger lst)
  (eliminate-larger-aux lst empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-nth-aux lst n count)
  (if (empty? lst)
      0
      (if (= count n)
          (first lst)
          (get-nth-aux (rest lst) n (+ 1 count))))) 
  

(define (get-nth lst n)
  (get-nth-aux lst n 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (find-item-aux lst target count)
  (if (empty? lst)
      -1
      (if (= (first lst) target)
          count
          (find-item-aux (rest lst) target (+ 1 count)))))

(define (find-item lst target)
  (find-item-aux lst target 0))
      
  


  
      
      
  
  
      