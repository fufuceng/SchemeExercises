#lang racket
;; Product Perfect Number is the number which equals to the product of all of its divisors except for itself.
(define divisible?
  (lambda (n a)
    (cond
      ((= n 0) true)
      ((= a 0) false)
      ((= a 1) true)
      ((= (modulo n a) 0) true)
      (else false))))

(define product_of_divisors
  (lambda (n a)
    (cond
      ((= n a) 1)
      (else (cond
         ((divisible? n a) (* (product_of_divisors n (+ a 1)) a))
         (else (product_of_divisors n (+ a 1))))))))

(define perfect?
  (lambda (n)
    (cond
      ((= (product_of_divisors n 2) n) true)
      (else false))))

(perfect? 6)
(perfect? 46)
(perfect? 39281)
(perfect? 9)
(perfect? 45)