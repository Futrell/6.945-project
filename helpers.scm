(define (compose f g)
  (lambda (x) (f (g x))))

(define (identity x)
  x)

(define (iterate f n)
  (if (= n 0)
      identity
      (compose f (iterate f (- n 1)))))
