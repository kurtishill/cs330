#lang racket

(define (default-parms f values)
  (lambda args
    (apply f (append args (list-tail values (length args))))))
    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (type-helper lst-val lst-parms)
  (andmap (lambda (v p) (p v)) lst-val lst-parms))
      

(define (type-parms f types)
  (lambda values
    (if (type-helper values types)
        (apply f values)
        (error "ERROR MSG"))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (degrees-to-radians angle)
  (/ (* angle pi) 180))

(define (new-sin angle type)
  (cond
    ((symbol=? type 'radians) (sin angle))
    (else (sin (degrees-to-radians angle)))))

(define new-sin2 (default-parms
                   (type-parms
                    new-sin
                    (list number? symbol?))
                   (list 0 'radians)))
                     
      