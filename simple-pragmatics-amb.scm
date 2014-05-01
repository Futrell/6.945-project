;;; Let's try to do the simple pragmatics model using amb. (After that
;;; we can try to do propagators!).

;;; Basically here we just use the amb mechanism to do rejection
;;; sampling. 

(load "helpers")

(define *sample-size* 5)

; Church compatibility:
(define pair cons)
(define first car)
(define second cadr)

(define (random-choice lst)
  (list-ref lst (random (length lst))))

(define (match-procedure thing-to-query thing-to-get)
  (define (matches alist thing)
    (define (find-matches alist found-so-far)
      (if (null? alist)
          found-so-far
          (let ((first-pair (first alist)))
            (if (eq? (thing-to-query first-pair) thing)
                (find-matches (rest alist)
                              (pair (thing-to-get first-pair) found-so-far))
                (find-matches (rest alist)
                              found-so-far)))))
    (find-matches alist '()))
  matches)

(define matches (match-procedure first second))
(define reverse-matches (match-procedure second first))

(define (make-literal-speaker lexicon)
  (lambda (meaning)
    (random-choice (matches lexicon meaning))))

(define (make-literal-listener lexicon)
  (lambda (message)
    (random-choice (reverse-matches lexicon meaning))))

(define (make-pragmatic-listener meaning->message)
  (lambda (message)
    (let ((meaning (amb universe))) ; rejection sampling
      (if (eq? (meaning->message meaning) message)
          meaning
          (amb)))))

(define (make-pragmatic-speaker message->meaning meaning->message)
  (lambda (meaning)
    (let ((possible-messages
           ((iterate meaning->message *sample-size*) meaning)))
      (let (message (amb possible-messages)) ; rejection sampling
        (if (eq? (message->meaning message) meaning)
            message
            (amb))))))
