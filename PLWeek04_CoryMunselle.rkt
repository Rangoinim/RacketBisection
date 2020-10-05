#lang racket

; Polynomial 5x^2 - x - 8
(define (myPolynomial x)
  [- (- [* 5 (* x x) ] x) 8])

; Polynomial 4x^2 - x - 2
(define (mySecPolynomial x)
  [- (- [* 4 (* x x) ] x) 2])

; Helper function to calculate average
(define (calcAverage x y)
  (/ [+ x y] 2))

; Calculate bisections until a sufficient value is met based on given precision
(define (bisect poly num1 num2 accuracy)
  ; if (poly1 < 0 and poly2 > 0) or (poly1 > 0 and poly2 < 0)
  (if (or (and (< [poly num1] 0) (> [poly num2] 0)) (and (> [poly num1] 0) (< [poly num2] 0)))
      ; if (poly1 * polyAvg) < 0 and |polyAvg| > accuracy
      (if [and (< (* (poly num1) (poly [calcAverage num1 num2])) 0) (> (abs [poly (calcAverage num1 num2)]) accuracy)]
          ; recurse, replacing num2 with average of num1 & num2
          (bisect poly num1 (calcAverage num1 num2) accuracy)
          ; else recurse, replacing num1 with average of num1 & num2
          (bisect poly (calcAverage num1 num2) num2 accuracy))
      ; else finish with average for final calculation
      (calcAverage num1 num2)))

(exact->inexact (myPolynomial (bisect myPolynomial 2 1 0.1)))

(exact->inexact (mySecPolynomial (bisect mySecPolynomial -3 0 0.1)))