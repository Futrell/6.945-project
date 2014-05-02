;;;; beal.scm

;;; This is an implementation of Beal's paper using our generic
;;; communication tools, which were themselves based on Beal's paper.

(load "sketch-2.scm")

;;;; Utils

(define (coin) (equal? (random 2) 1))
(define (get-random lst) (list-ref lst (random (length lst))))
(define (wcoin p) (< (random 1.0) p)) ;weighted coin


;;;; DICTIONARIES

(define tenses
  '(simple-present present-progressive simple-past past-progressive
    present-perfect-simple present-perfect-progressive
    past-perfect-simple past-perfect-progressive future-i-simple
    future-i-simple-going-to future-i-progressive future-ii-simple
    future-ii-progressive conditional-i-simple
    conditional-i-progressive conditional-ii-simple
    conditional-ii-progressive))
(define general-verbs '(be have use make find get go come))
(define animate-verbs '(look write see))
(define verbs (append general-verbs animate-verbs))
(define animate-nouns
  '(you it he they I she this that we she Richard James people))
(define inanimate-nouns '(word time number way water oil day part))
(define nouns (append animate-nouns inanimate-nouns))
(define places '(here there in out up down))
(define types '(question affermative negative))
(define words (append tenses verbs nouns places types))

(define P_animate 0.5)
(define P_place 0.2)
(define P_type 0.2)
(define P_tense 0.2)

(define (sample-universe-event)
  (let* ((animate? (wcoin P_animate))
         (object (get-rand nouns))
         (subject (if animate? 
                      (get-random animate-nouns) 
                      (get-random nouns)))
         (verb (if animate?
                   (get-random animate-verbs)
                   (get-random general-verbs)))
         (event `((verb ,verb) (object ,object) (subject ,subject))))
    (if (wcoin P_place)
      (set! event (cons (list 'place (get-random places)) event)))
    (if (wcoin P_type)
      (set! event (cons (list 'type (get-random types)) event)))
    (if (wcoin P_tense)
      (set! event (cons (list 'tense (get-random tenses)) event)))
    event))

;;; BEAL SETUP

(define num-comm-lines 10000)

(define comm-line-perm '(head))
(define (add-to-perm n)
  (if (not (= n num-comm-lines))
      (begin
        (let* ((cur-len (- (length comm-line-perm) 1))
               (insert-at (random (+ 1 cur-len)))
               (tail (list-tail comm-line-perm insert-at)))
          (set-cdr! tail (cons n (cdr tail))))
        (add-to-perm (+ n 1)))))
(add-to-perm 0)
(set! comm-line-perm (cdr comm-line-perm))
(define back-perm (make-list num-comm-lines))
(define (add-to-back-perm n)
  (if (not (= n num-comm-lines))
      (begin
        (list-set-at! back-perm (list-ref comm-line-perm n) n)
        (add-to-back-perm (+ n 1)))))
(add-to-back-perm 0)



;;; LANGUAGE

;;; (symbol, inflection)

(define (make-simple-language parent-agent) 
  (define grammar (list (dict) (dict)))
  (define symbol-grammar first)
  (define inflection-grammar second)
  ;; The grammar is a list of two dictionaries. 
  ;;   The first dictionary describes the observed symbols.
  ;;     Keys are observed symbols.
  ;;     Values are a list of two lists.
  ;;       The first sub-list is a list of uncertain lines.
  ;;       The second is a list of certain lines.
  ;;   The second dictionary describes the observed inflections.
  ;;     Keys are observed inflections.
  ;;     Values are real number estimates of the inflection mappings.

;Beal drives lines with 1s and -1s, and the percentage of 1s is the
;inflection mapping. I guess for multiple items conflicts are marked
;as conflicts, and provide a range of values? If we have 6
;inflections, we want around 18-30 lines per symbol to accurately see
;them. That's 0.0018 in intersection, so about 500-600 lines
;originally for each. 

  (define (add-meaning-message-pair-to-grammar! meaning message)
    (let* ((meaning-symbol (first meaning))
           (meaning-inflection (second meaning))
           (
    (pp "new grammar:")
    (pp grammar))

  (define respond-to-feedback! add-meaning-message-pair-to-grammar!)

  (define (dont-know-message-for-meaning meaning)
    (let ((made-up-message (gensym)))
      (add-meaning-message-pair-to-grammar! meaning made-up-message)
      made-up-message))

  (define (dont-know-meaning-of-message message)
    'i-dont-know) ; give up

  (define (update-grammar! feedback)
    (if (not (eq? feedback 'ok))
        (begin
          (apply respond-to-feedback! feedback))))

  (define (meaning->message meaning parameters)
    (let ((message (lookup-ref meaning grammar 0))
          (agent-to-speak (car parameters)))
      (if (eq? agent-to-speak (parent-agent))
          (if message
              message
              (dont-know-message-for-meaning meaning))
          silence)))

  (define (message->meaning message parameters)
    (let ((meaning (lookup-ref message grammar 1))
          (agent-to-speak (car parameters)))
      (if (not (eq? agent-to-speak (parent-agent)))
        (if meaning
            meaning
            (dont-know-meaning-of-message message))
        silence)))



  (make-language grammar
                 meaning->message
                 message->meaning
                 update-grammar!
                 parent-agent))

;;; AGENT

(define (basic-feedback-proc agent event rec-message interp params)
  (if (and (eq? (cadr params) 'training)
           (not (eq? interp silence))
           (not (eq? interp event)))
      (list ((get-perceive-proc agent) event) rec-message)
      'ok))

(define (make-simple-agent)
  (make-agent make-simple-language
              (lambda (x) x)
              (lambda (x) x)
              make-basic-history
              basic-feedback-proc))


;;; CHANNEL

(define (channel-transmit channel)
  (let* ((a1-message 
          (get-last-message-out 
            (get-history (get-channel-agent1 channel))))
         (a2-message
          (get-last-message-out
            (get-history (get-channel-agent2 channel))))
         (a1-transmitted 
          (transmit (get-channel-model channel) a1-message))
         (a2-transmitted
          (transmit (get-channel-model channel) a2-message)))
    (set-last-message-in! 
      (get-history (get-channel-agent2 channel))
      channel
      a1-transmitted)
    (set-last-message-in!
      (get-history (get-channel-agent1 channel))
      channel
      a2-transmitted)))


;;; EXPERIMENTAL SETUP

(define number-of-training-runs 5)
(define number-of-test-runs 5)

(define (make-simple-experiment)
  (let* ((agent-1 (make-simple-agent))
         (agent-2 (make-simple-agent))
         (channel (make-channel noiseless-channel agent-1 agent-2)))
    (make-experiment (list agent-1 agent-2)
                     (list channel)
                     sample-universe-event)))
