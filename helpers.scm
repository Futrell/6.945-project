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

(define (list-set-at! lst n val)
  (set-car! (list-tail lst n) val)) 
  ;; http://stackoverflow.com/questions/7382117

(define dict-end '(end# end#))

(define (dict) (list dict-end))

(define (dict-put! alist key val) 
  ;; Adds a value to a key, replacing previous values for
  ;; the key 
  (if (and (assq key alist) 
           (not (eq? (assq key alist) dict-end)))
      (let ((dict-mem (member-procedure 
                        (lambda (ele obj) (eq? obj (car ele))))))
        (set-car! (dict-mem key alist) (list key val)))
      (begin
        (set-car! (list-tail alist (- (length alist) 1))
                  (list key val))
        (set-cdr! (list-tail alist (- (length alist) 1))
                  (list dict-end)))))

(define gensym generate-uninterned-symbol)

(define (reverse items)
  (fold-right (lambda (x r) (append r (list x))) '() items))

(define (tap x)
  ;; Print then return; useful for debugging.
  (pp x)
  x)

(define (random-choice lst)
  (list-ref lst (random (length lst))))

