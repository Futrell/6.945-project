;;; Interface requires implementations of:
;;;
;;; 1. make-literal-speaker lexicon
;;; 2. make-literal-listener lexicon
;;; 3. make-pragmatic-speaker message->meaning
;;; 4. make-pragmatic-listener meaning->message
;;;
;;; Those can be implemented in Church or with propagators or whatever
;;; (I will try to do that now). So this interface needs to be legal
;;; Scheme code as well as Church code. 

(load "helpers")

;;; This stuff should probably be elsewhere:
(define (make-language-interface meaning->message message->meaning)
  (vector meaning->message message->meaning))

(define (language-interface-meaning->message language-interface)
  (vector-ref language-interface 0))

(define (language-interface-message->meaning language-interface)
  (vector-ref language-interface 1))

(define (make-literal-speaker lexicon)
  (lambda (meaning)
    'IMPLEMENT-ME))

(define (make-literal-listener lexicon)
  (lambda (message)
    'IMPLEMENT-ME))

(define (make-pragmatic-listener meaning->message)
  'IMPLEMENT-ME)

(define (make-pragmatic-speaker message->meaning)
  'IMPLEMENT-ME)

(define (make-literal-speaker-language-interface lexicon)
  (let ((speaker (make-literal-speaker lexicon)))
    (make-language-interface
     speaker
     (make-pragmatic-listener speaker))))

(define (make-literal-listener-language-interface lexicon)
  (let ((listener (make-literal-listener lexicon)))
    (make-language-interface
     (make-pragmatic-speaker listener)
     listener)))

(define (make-pragmatic-language-interface language-interface)
  (make-language-interface
   (make-pragmatic-speaker (language-interface-message->meaning
                            language-interface))
   (make-pragmatic-listener (language-interface-meaning->message
                             language-interface))))

(define (make-pragmatic-language-interface-with-depth n base)
  ((iterate make-pragmatic-language-interface n) base))

#| Now you can do stuff like:

(make-pragmatic-language-interface
 (make-pragmatic-language-interface
  (make-pragmatic-language-interface
   (make-pragmatic-language-interface
    (make-pragmatic-language-interface
     (make-literal-listener-language-interface lexicon))))))

Which will probably be horrifyingly slow!

|#
