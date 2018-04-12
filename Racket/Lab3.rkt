#lang racket

(define (curry2 func)
  (lambda (x)
    (lambda (y)
      (func x y))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (F-C temp)
  (* (- temp 32) (/ 5 9)))

(define (convertFC temps)
  (map F-C temps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-temps1 temps)
  (andmap (lambda (x) (and (<= 5 x) (>= 95 x))) temps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-temps temps low high)
  (andmap (lambda (x) (and (<= low x) (>= high x))) temps))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (convert digits)
  (foldr (lambda (x y) (+ (* y 10) x)) 0 digits))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-duple x)
  (list x x))

(define (duple lst)
  (map make-duple lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sum lst)
  (foldr + 0 lst))

(define (average lst)
  (/ (sum lst) (length lst)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compare-values lst num)
  (andmap (lambda (x) (<= num x)) lst))

(define (eliminate-larger lst)
  (foldr (lambda (x l)
           (if (compare-values l x)
               (cons x l)
               l)) '() lst)) 