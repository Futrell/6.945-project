(define (compose f g)
  (lambda (x) (f (g x))))

(define (identity x)
  x)

(define (iterate f n)
  (if (= n 0)
      identity
      (compose f (iterate f (- n 1)))))

(define (do-n-times f n)
  (lambda args
    (define (do-it n)
      (if (= n 0)
          '()
          (cons (apply f args)
                (do-it (- n 1)))))
    (do-it n)))

#| Tests

((do-n-times (lambda (x) x) 3) 'cat)
;Value: (cat cat cat)

|#

(define (count-true pred lst)
  (if (null? lst)
      0
      (+ (if (pred (car lst)) 1 0)
         (count-true pred (cdr lst)))))

#| Tests

(count-true (lambda (x) (> x 5)) '(1 2 5 6))
;Value: 1

(count-true (lambda (x) (eq? x 'cat)) '(cat cat dog dog dog cat))
;Value: 3

|#
