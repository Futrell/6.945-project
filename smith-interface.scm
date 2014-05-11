;;; This is an interface to aid in the creation of agents as described
;;; in Smith, Goodman & Frank (2013). A "language interface" in this
;;; code refers to a pair of procedures meaning->message and
;;; message->meaning.

;;; Smith agents encode and decode messages by one of two possible
;;; processes:
;;;
;;; 1. Literal process: They look up a meaning or a message in a fixed
;;; lexicon, which provides a (not 1-to-1!) mapping of meanings to
;;; messages and vice versa.
;;;
;;; 2. Pragmatic process: They reason about what meaning would have
;;; caused a speaker to emit a message, or about what message would be
;;; most likely to cause a hearer to arrive at the correct
;;; message.
;;;
;;; The "speakers" and "hearers" that the pragmatic agents reason
;;; about can be either literal speakers/hearers, or pragmatic
;;; speakers/hearers. 

;;; The interface requires implementations of:
;;;
;;; 1. (make-literal-speaker lexicon)
;;; 2. (make-literal-listener lexicon)
;;; 3. (make-pragmatic-speaker message->meaning meaning->message)
;;; 4. (make-pragmatic-listener meaning->message)
;;;
;;; Those can be implemented in Church or with amb. In order to make
;;; Church more feasible, this interface needs to be legal Scheme code
;;; as well as Church code (i.e., no mutation, no define-structure,
;;; car/cdr/cons=first/rest/pair).

(load "helpers")

(define (make-language-interface meaning->message message->meaning)
  (vector meaning->message message->meaning))

(define (language-interface-meaning->message language-interface)
  (vector-ref language-interface 0))

(define (language-interface-message->meaning language-interface)
  (vector-ref language-interface 1))

;;; A literal speaker is a meaning->message procedure that takes a
;;; meaning and looks it up in a lookup table (the lexicon). The
;;; message is just the result of that lookup.
(define (make-literal-speaker lexicon)
  (lambda (meaning)
    'IMPLEMENT-ME))

;;; A literal listener is a message->meaning procedure that takes a
;;; message, looks it up in a lookup table, and just returns the
;;; meaning that it finds.
(define (make-literal-listener lexicon)
  (lambda (message)
    'IMPLEMENT-ME))

;;; A pragmatic listener is a message->meaning procedure that takes a
;;; message and infers the meaning that was most likely to produce
;;; that message according to the meaning->message procedure that you
;;; pass to this constructor.
(define (make-pragmatic-listener meaning->message)
  (lambda (message)
    'IMPLEMENT-ME))

;;; A pragmatic speaker is a meaning->message procedure that figures
;;; out the message that is most likely to lead to the correct
;;; interpretation according to the message->meaning procedure you
;;; pass to this constructor.
;;;
;;; Interesting subtlety: This constructor also requires a
;;; meaning->message procedure. This is because we assume that there
;;; is a procedure for sampling universe events, but no procedure for
;;; sampling possible messages. So to sample possible messages, you
;;; have to sample possible events and then turn them into messages
;;; according to a meaning->message procedure. This might be an
;;; interesting philosophical point that stems from our assumption
;;; that languages are inherently mappings from meanings to
;;; messages. (Or it might just be wrong!)
(define (make-pragmatic-speaker message->meaning meaning->message)
  (lambda (meaning)
    'IMPLEMENT-ME))

;;; This is a convenience function for building a common base case: a
;;; language interface where the meaning->message procedure is literal
;;; and the message->meaning procedure is pragmatic.
(define (make-literal-speaker-language-interface lexicon)
  (let ((speaker (make-literal-speaker lexicon)))
    (make-language-interface
     (make-literal-speaker lexicon)
     (make-pragmatic-listener speaker))))

;;; Another convenience function for a common base case. Here the
;;; meaning->message procedure is pragmatic and the message->meaning
;;; procedure is literal. (This is the base case in the Smith model
;;; and related papers.)
(define (make-literal-listener-language-interface lexicon)
  (let ((listener (make-literal-listener lexicon)))
    (make-language-interface
     (make-pragmatic-speaker listener (make-literal-speaker lexicon))
     listener)))

;;; This builds a language-interface with pragmatic message->meaning
;;; and pragmatic meaning->message on top of another language interface.
(define (make-pragmatic-language-interface language-interface)
  (let ((underlying-listener
         (language-interface-message->meaning language-interface))
        (underlying-speaker
         (language-interface-meaning->message language-interface)))
    (let ((listener (make-pragmatic-listener underlying-speaker)))
      (make-language-interface
       (make-pragmatic-speaker listener underlying-speaker)
       listener))))

(define (make-pragmatic-language-interface-with-depth n base)
  ((iterate make-pragmatic-language-interface n) base))

#| Now we can do this kind of thing!

(make-pragmatic-language-interface
 (make-pragmatic-language-interface
  (make-pragmatic-language-interface
   (make-pragmatic-language-interface
    (make-pragmatic-language-interface
     (make-literal-listener-language-interface lexicon))))))

|#
