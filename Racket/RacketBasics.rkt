#lang racket

(define (sum-coins pennies nickels dimes quarters)
  (+ (* 1 pennies) (* 5 nickels) (* 10 dimes) (* 25 quarters)))

(define (degrees-to-radians angle)
  (/ (* angle pi) 180))

(define (sign x)
  (cond
    ((< x 0) -1)
    ((> x 0) 1)
    (else 0)))

(define (new-sin angle type)
  (cond
    ((symbol=? type 'radians) (sin angle))
    (else (sin (degrees-to-radians angle)))))