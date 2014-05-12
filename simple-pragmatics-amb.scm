;;; This is an attempt to fill in the Smith interface using amb to do
;;; inference by rejection sampling.

(load "helpers")
(load "stack-queue")
(load "ambsch")

(define *sample-size* 100) ; Number of rejection samples to draw.

; Church compatibility:
(define pair cons)
(define rest cdr)

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


(define (sample-universe-event)
  (random-choice universe))

(define (make-literal-speaker lexicon)
  (lambda (meaning)
    (random-choice (matches lexicon meaning))))

(define (make-literal-listener lexicon)
  (lambda (message)
    (random-choice (reverse-matches lexicon message))))

#| Tests

(load "simple-pragmatics-test")
(define listener (make-literal-listener lexicon))
(define speaker (make-literal-speaker lexicon))
(define language (make-language-interface speaker listener))
((language-interface-meaning->message language) 'cat)
;Value: katze

((language-interface-message->meaning language) 'katze)
;Value: cat

(load "simple-pragmatics-test-ambiguous")
(define listener (make-literal-listener lexicon))
(define speaker (make-literal-speaker lexicon))
(define language (make-language-interface speaker listener))
((language-interface-meaning->message language) 'man)
;Value: man
;Value: man-with-nothing

(count-true (lambda (x) (eq? x 'man)) ((do-n-times listener 100) 'man))
;Value: 32
Usually these values are in the vicinity of 32.

((language-interface-message->meaning language) 'man)
;Value: man-with-nothing
;Value: man-with-only-glasses
;Value: man-with-hat-and-glasses

|#

(define (make-pragmatic-listener meaning->message)
  (lambda (message)
    (let ((possible-meanings
           ((do-n-times sample-universe-event *sample-size*))))
      (let ((meaning (amb-from-list possible-meanings)))
        (require (eq? (meaning->message meaning) message))
        meaning))))

(define (make-pragmatic-speaker message->meaning meaning->message)
  (lambda (meaning)
    (let ((possible-messages
           ((do-n-times meaning->message *sample-size*) meaning)))
      (let ((message (amb-from-list possible-messages))) ; rejection sampling
        (require (eq? (message->meaning message) meaning))
        message))))

#| Tests

(load "simple-pragmatics-test-ambiguous")
(init-amb)

(define speaker (make-literal-speaker lexicon))
(define p-listener (make-pragmatic-listener speaker))
(define pragmatic-listener-language (make-language-interface speaker p-listener))
((language-interface-message->meaning pragmatic-listener-language) 'man)
;Value: man

(count-true (lambda (x) (eq? x 'man)) ((do-n-times p-listener 100) 'man))
;Value: 49
Usually these values are in the vicinity of 50: the listener
interprets "man" as "man with neither hat nor glasses" more often
than the literal listener does.

(p-listener 'man-with-glasses)
;Value: man-with-hat-and-glasses

(count-true (lambda (x) (eq? x 'man-with-only-glasses)) ((do-n-times p-listener 100) 'man-with-glasses))
;Value: 61

(define listener (make-literal-listener lexicon))
(define speaker (make-literal-speaker lexicon))
(define p-speaker (make-pragmatic-speaker listener speaker))
(define pragmatic-speaker-language (make-language-interface p-speaker listener))
((language-interface-message->meaning pragmatic-listener-language) 'man-with-only-glasses)
;Value: man-with-only-glasses


These examples show how the pragmatic speaker avoids ambiguous messages:

(count-true (lambda (x) (eq? x 'man-with-nothing)) ((do-n-times speaker 100) 'man))
;Value: 55

(count-true (lambda (x) (eq? x 'man-with-nothing)) ((do-n-times p-speaker 100) 'man))
;Value: 76

(count-true (lambda (x) (eq? x 'man-with-only-glasses))
            ((do-n-times speaker 100) 'man-with-only-glasses))
;Value: 34

(count-true (lambda (x) (eq? x 'man-with-only-glasses))
            ((do-n-times p-speaker 100) 'man-with-only-glasses))
;Value: 55

Now for the fun part. What about when a pragmatic speaker knows
he's talking to a pragmatic listener?

(define pragmatic-language
  (make-pragmatic-language-interface
   (make-literal-listener-language-interface lexicon)))

(define pp-speaker (language-interface-meaning->message pragmatic-language))
(define pp-listener (language-interface-message->meaning pragmatic-language))

(count-true (lambda (x) (eq? x 'man-with-nothing))
            ((do-n-times pp-speaker 50) 'man))
;Value: 38

(count-true (lambda (x) (eq? x 'man-with-glasses))
            ((do-n-times pp-speaker 50) 'man-with-hat-and-glasses))
;Value: 11

The pragmatic speaker knows to avoid calling the man with the hat and glasses
"the man with glasses".

Let's make it one level deeper. (Experimental evidence suggests that this is
the maximal depth for human language use.) The amb-rejection-sampling
mechanism now becomes very slow and starts causing us to run out of memory...

(define pragmatic-pragmatic-language
  (make-pragmatic-language-interface
   (make-pragmatic-language-interface
    (make-literal-listener-language-interface lexicon))))

(define ppp-speaker (language-interface-meaning->message
                     pragmatic-pragmatic-language))
(define ppp-listener (language-interface-message->meaning
                     pragmatic-pragmatic-language))

(count-true (lambda (x) (eq? x 'man-with-only-glasses))
            ((do-n-times ppp-listener 1) 'man-with-glasses))
  
;Value: 6

So it's not clear if these agents are able to figure out that when
a pragmatic speaker says "man with glasses", it means the man with ONLY
glasses, not the man with glasses and a hat.

|#
