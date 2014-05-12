;;;; beal.scm
;;; This is an implementation of Beal's paper using our generic
;;; communication tools, which were themselves based on Beal's paper.
;;; See the README for an overview of concepts used in this code. 

(load "experiment.scm")

;;;; UTILS

;;; Returns #t or #f with 0.5 probability.
(define (coin) (equal? (random 2) 1))

;;; Returns a random element of a list.
(define (get-random lst) (list-ref lst (random (length lst))))

;;; Weighted coin returns #t with probability p; #f otherwise.
(define (wcoin p) (< (random 1.0) p))


;;;; UNIVERSE
;;; This section defines variables and functions involved in
;;; generating universe events.

;;; Lists of words used for generating events.
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

;;; Probabilities used for generating events.
(define P_animate 0.5) ;Probability an event contains a verb that
		       ;requires an animate noun subject.
(define P_place 0.2) ;Probability place information is specified.
(define P_type 0.2) ;Probability type (question, etc) is specified.
(define P_tense 0.2) ;Probability tense is specified.

;;; Generates a random event. Each event includes a subject, object
;;; and verb, and possibly a place, type, or tense. Some verbs can
;;; only be used with animate nouns.
(define (sample-universe-event)
  (let* ((animate? (wcoin P_animate))
         (object (get-random nouns))
         (subject (if animate? 
                      (get-random animate-nouns) 
                      (get-random nouns)))
         (verb (if animate?
                   (get-random animate-verbs)
                   (get-random general-verbs)))
         (event `((,verb verb) (,object object) (,subject subject))))
    (if (wcoin P_place)
      (set! event (cons (list (get-random places) 'place) event)))
    (if (wcoin P_type)
      (set! event (cons (list (get-random types) 'type) event)))
    (if (wcoin P_tense)
      (set! event (cons (list (get-random tenses) 'tense) event)))
    event))


;;;; BEAL SETUP
;;; This section defines functions and variables that are used by the
;;; comm channel and agents relating to Beal's encoding scheme.

;;; The number of communication lines in a channel between agents.
(define num-comm-lines 10000)

;;; comm-line-perm is a list of integers that form a permutation from
;;; 0 to (- num-comm-lines 1). This is used to shuffle the messages
;;; between agents. back-perm is the inverse permutation.
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



;;;; LANGUAGE
;;; This section defines the language used by the agents.


;;; Comm lines are stored in two ways. First as lists of integers from
;;; 0 to num-comm-lines, and second as dictionaires of comm lines that
;;; are driven and the values they are driven at. 

;;; Approximately the number of comm lines initially associated with a
;;; symbol that will be driven when that symbol occurs in an event.
;;; Since num-comm-lines is 10000, the intersection of two sets of
;;; comm lines has about 100 lines on average.
(define initial-comm-lines-per-agent-symbl 600)

;;; comm lines are chosen randomly so that about the above number are
;;; chosen. 
(define prob-initial-comm-line
  (/ (+ 0.0 initial-comm-lines-per-agent-symbl)
     (+ 0.0 num-comm-lines)))

;;; How close inflection mappings can be without having to re-guess.
(define move-closer 0.1)

;;; The number of comm lines associated with a symbol that can be
;;; undriven and still have the symbol considered a match.
(define closeness-allowed 10)

;;; Like dict-put!, but sets conflicting comm line values to 'X, with
;;; the exception of 0 which can be overwritten.
(define (comm-lines-put! alist key val) 
  ;; Adds a value to a key, replacing previous values for
  ;; the key 
  (if (and (assq key alist) 
           (not (eq? (assq key alist) dict-end)))
      (let ((dict-mem (member-procedure 
                        (lambda (ele obj) (eq? obj (car ele))))))
        (if (or (eq? (cadr (assq key alist)) val)
                (eq? (cadr (assq key alist)) 0))
            (set-car! (dict-mem key alist) (list key val))
            (set-car! (dict-mem key alist) (list key 'X)) ))
      (begin
        (set-car! (list-tail alist (- (length alist) 1))
                  (list key val))
        (set-cdr! (list-tail alist (- (length alist) 1))
                  (list dict-end)))))

;;; Merge two dictionaries of comm-lines and their values.
(define (comm-lines-merge c1 c2)
  (define new-line-set (dict))
  (define (helper comm-line-set)
    (if (not (eq? (car comm-line-set) dict-end))
        (begin 
          (comm-lines-put! new-line-set
                           (caar comm-line-set)
                           (cadar comm-line-set))
          (helper (cdr comm-line-set)))))
  (helper c1)
  (helper c2)
  new-line-set)

;;; Associate values with comm lines, choosing inflection-prob
;;; fraction random elements to be 1 and all others -1.
(define (comm-lines-choose-vals lines inflection-prob)
  (define line-val-set (dict))
  (define num-1s (floor->exact (* (length lines) inflection-prob)))
  (if (wcoin (- (* (length lines) inflection-prob) num-1s))
                           ; ensuring correct expected number of lines 
    (set! num-1s (+ num-1s 1)))
  (define (helper-1 line-set) ; add lines to dictionary
    (if (null? line-set)
        line-val-set
        (let ((to-add (car line-set)))
          (dict-put! line-val-set to-add -1)
          (helper-1 (cdr line-set)))))
  (set! line-val-set (helper-1 lines))
  (define (helper-2 num-1s-to-add) ; set lines to 1 randomly
    (if (eq? num-1s-to-add 0)
        line-val-set
        (let ((to-add (get-random lines)))
          (if (= (cadr (assq to-add line-val-set)) -1)
              (begin
                (dict-put! line-val-set to-add 1)
                (helper-2 (- num-1s-to-add 1))) 
              (helper-2 num-1s-to-add)))))
  (helper-2 num-1s))
#|
1 ]=> (comm-lines-choose-vals '(0 1 2 3 4 5 6 7 8 9) 0.525)
;Value 17: ((0 -1) (1 -1) (2 1) (3 1) (4 1) (5 -1) (6 1) (7 1) (8 1) (9 -1) (end# end#))
1 ]=> (comm-lines-choose-vals '(0 1 2 3 4 5 6 7 8 9) 0.525)
;Value 18: ((0 1) (1 -1) (2 1) (3 -1) (4 -1) (5 -1) (6 1) (7 1) (8 -1) (9 1) (end# end#))
1 ]=> (comm-lines-choose-vals '(0 1 2 3 4 5 6 7 8 9) 0.525)
;Value 19: ((0 -1) (1 -1) (2 -1) (3 1) (4 1) (5 1) (6 1) (7 1) (8 -1) (9 -1) (end# end#))
1 ]=> (comm-lines-choose-vals '(0 1 2 3 4 5 6 7 8 9) 0.525)
;Value 20: ((0 -1) (1 -1) (2 1) (3 1) (4 1) (5 1) (6 -1) (7 1) (8 -1) (9 -1) (end# end#))
|#


;;; Generate a random set of comm lines (list of integers) by choosing
;;; each possible comm line with probability prob-initial-comm-line.
(define (random-comm-set)
  (define (helper line-set i)
    (if (= i 0)
      line-set
      (if (wcoin prob-initial-comm-line)
          (helper (cons (- i 1) line-set) (- i 1))
          (helper line-set (- i 1)))))
  (helper '() num-comm-lines))

;;; Finds intersection of two sets of comm lines as lists of integers.
(define (comm-sets-intersect c1 c2)
  (define (helper c1-sublst retlst)
    (if (null? c1-sublst)
        retlst
        (if (member (car c1-sublst) c2)
            (helper (cdr c1-sublst) (cons (car c1-sublst) retlst))
            (helper (cdr c1-sublst) retlst))))
  (helper c1 '()))

;;; Removes the values from a dictionary of comm lines to values.
;;; Returns a list of integers.
(define (extract-lines message)
  (if (eq? (car message) dict-end) ;not worrying about efficiency
      '()
      (cons (caar message) (extract-lines (cdr message)))))

;;; Counts numbers of 1s and Xs in a set of lines within a message.
;;; Returns the numbers as a pair.
(define (count-1s-xs lines message)
  (define (helper lines ones-xs)
    (if (null? lines)
        ones-xs
        (let* ((line (car lines))
               (val (cadr (assq line message)))
               (ones (car ones-xs))
               (xs (cdr ones-xs)))
          (cond
            ((equal? val 1)
             (helper (cdr lines) (cons (+ 1 ones) xs)))
            ((equal? val -1) 
             (helper (cdr lines) (cons ones xs)))
            ((eq? val 'X) 
             (helper (cdr lines) (cons ones (+ 1 xs))))))))
  (helper lines '(0 . 0)))

;;; This function makes the language object, containing a grammar and
;;; the functions required from the language by the experiment in
;;; sketch-2.scm.
(define (make-beal-language parent-agent) 
  (define grammar (list (dict) (dict)))
  (define symbol-grammar first)
  (define inflec-grammar second)
  ;; The grammar is a list of two dictionaries. 
  ;;   The first dictionary describes the observed symbols.
  ;;     Keys are observed symbols.
  ;;     Values are lists of comm lines associated with the symbol.
  ;;   The second dictionary describes the observed inflections.
  ;;     Keys are observed inflections.
  ;;     Values are real number estimates of the inflection mappings.

  ;; Feedback is handled by message->meaning during the training phase
  (define (update-grammar! feedback)
    'ok)

  ;; Look up the set of comm lines for a symbol in the grammar. If it
  ;; is not found, generate a new random list of comm lines
  (define (lookup-sym-or-new-meaning symbol)
    (let ((sym-mess (assq symbol (symbol-grammar grammar))))
      (if (not sym-mess)
          (begin
            (dict-put! (symbol-grammar grammar) 
                       symbol 
                       (random-comm-set))
            (cadr (assq symbol (symbol-grammar grammar))))
          (cadr sym-mess))))

  ;; Look up the inflection mapping estimate in the grammar. If it is
  ;; not found, generate a new random inflection mapping.
  (define (lookup-inf-or-new-meaning inflec)
    (let ((inf-mess (assq inflec (inflec-grammar grammar))))
      (if (not inf-mess)
          (begin
            (dict-put! (inflec-grammar grammar) 
                       inflec
                       (random 1.0))
            (cadr (assq inflec (inflec-grammar grammar))))
          (cadr inf-mess))))

  (define count 0) ;Used for test output
  (define (llength x)
    (if (list? x)
        (length x)
        0))

  ;; Convert the meaning interpreted from the universe event into a
  ;; message (a dictionary of driven comm lines to values).
  (define (meaning->message meaning parameters)
    (define message (dict))
    (set! count (+ count 1)) ;Used for test output
    (if (or (eq? (cadr parameters) 'testing) );(= (modulo count 1000) 0))
        (tap (list meaning ;(cadr grammar)
    ;               (map (lambda (x)
    ;                      (list (car x) (llength (cadr x)))) 
    ;                    (car grammar)))))
        )))
    (define (helper meaning-lst) ;look up and combine the meaning
				 ;sets of comm lines and inflection
				 ;mappings for each symbol-inflection
				 ;pair.
      (if (null? meaning-lst)
          message
          (let* ((symbol (caar meaning-lst))
                 (inflec (cadar meaning-lst))
                 (sym-mess (lookup-sym-or-new-meaning symbol))
                 (inf-mess (lookup-inf-or-new-meaning inflec))
                 (sym-comm-lines (dict)))
            (set! message 
                  (comm-lines-merge message
                         (comm-lines-choose-vals sym-mess inf-mess)))
            (helper (cdr meaning-lst)))))
    (let* ((agent-to-speak (car parameters))
           (mode (cadr parameters)))
      (cond ((eq? mode 'training)
             (helper meaning))
            ((eq? agent-to-speak (parent-agent))
             (helper meaning))
            (else message)))) ;Only the speaker's message is used in
			      ;the testing phase 

  ;; Given a received message and meaning, update the grammar to
  ;; reflect the new state of knowledge of the other agent's grammar
  (define (train message meaning) 
    (let* ((symbol (caar meaning))
           (inflec (cadar meaning))
           (comm-lines (extract-lines message))
           (expected-lines (lookup-sym-or-new-meaning symbol))
           ;; The lines this agent has assigned to the symbol.
           (matching-lines 
            (comm-sets-intersect comm-lines expected-lines))
           ;; The overlap between the lines this agent assigned and
	   ;; the driven message lines.
           (exp-inf-prob (lookup-inf-or-new-meaning inflec)))
           ;; What fraction of lines should be driven to 1.
      (if (null? matching-lines) ; If no lines are in common between
				 ; the other agent's message and this
				 ; agent's set of comm lines for the
				 ; symbol, generate a new random set
				 ; of comm lines.
          (dict-put! (symbol-grammar grammar)
                     symbol
                     (random-comm-set))
          (let* ((ones-xs (count-1s-xs matching-lines message))
                 (ones (car ones-xs))
                 (xs (cdr ones-xs))
                 (total (length matching-lines))
                 (lower (/ (+ ones 0.0) (+ total 0.0)))
                 (upper (/ (+ ones xs 0.0) (+ total 0.0)))
                  ;; Bounds on fraction of lines driven to 1.
                 (inf-map-give (/ 0.999 total)))
            (dict-put! (symbol-grammar grammar) ; The intersecting
                       symbol            ; lines are candidates for
                       matching-lines)   ; the other agent's line set.
            (cond
              ((and (<= (- lower inf-map-give) exp-inf-prob) 
                    (>= (+ upper inf-map-give) exp-inf-prob))
               'ok) ; This agent's inflection mapping is in the right
		    ; range.
              ((and (> lower exp-inf-prob) ;This agent's inflection
                    (< (- lower exp-inf-prob) move-closer));mapping is
               (dict-put! (inflec-grammar grammar)         ;too low.
                          inflec
                          (min 1 (+ exp-inf-prob
                                    (/ (- lower exp-inf-prob) 2.0)))))
                                 ;Divide by 2.0 to converge
              ((and (< upper exp-inf-prob) ;This agent's inflection
                    (< (- exp-inf-prob upper) move-closer));mapping is
               (dict-put! (inflec-grammar grammar)         ;too high
                          inflec
                          (max 0 (- exp-inf-prob
                                    (/ (- exp-inf-prob upper) 2.0)))))
              (else
               (dict-put! (inflec-grammar grammar)         ;too high
                          inflec
                          (random 1.0)))
            )))
      (if (not (null? (cdr meaning))) ;Train on the remaining
				      ;symbol-inflection pairs 
          (train message (cdr meaning)))))

  ;; Finds the inflection mapping estimate in this agent's grammar
  ;; closest to the average of a given range.  
  (define (find-inflec upper lower)
    (define avg (/ (+ upper lower) 2.0))
    (define (helper inflec inf-dict best-so-far)
      (if (eq? (car inf-dict) dict-end)
          (if inflec
              inflec
              'no-inflection-found)
          (if (< (abs (- (cadar inf-dict) avg)) best-so-far)
              (helper (caar inf-dict) 
                      (cdr inf-dict)
                      (abs (- (cadar inf-dict) avg)))
              (helper inflec (cdr inf-dict) best-so-far))))
    (helper #f (inflec-grammar grammar) 1.0))

  ;; Tries to find symbols and inflections from a dictionary of driven
  ;; message lines and values.  
  (define (lookup message)
    (let* ((mess-lines (extract-lines message)))
      (define (helper symbol-dict-tail meaning) ; Checks through all
					; known symbols in grammar.
        (if (eq? (car symbol-dict-tail) dict-end)
            meaning
            (let* ((symbol (caar symbol-dict-tail))
                   (symbol-lines (cadar symbol-dict-tail))
                   (matching-lines 
                    (comm-sets-intersect mess-lines symbol-lines)))
              (if (< (abs (- (length symbol-lines) ; If enough lines
                             (length matching-lines)))      ; match:
                     closeness-allowed)
                  (let* ((ones-xs 
                         (count-1s-xs matching-lines message))
                         (ones (car ones-xs))
                         (xs (cdr ones-xs))
                         (total (length matching-lines))
                         (lower (/ (+ ones 0.0) (+ total 0.0)))
                         (upper (/ (+ ones xs 0.0) (+ total 0.0))))
                    ;; Find the inflection and add the symbol
		    ;; inflection pair to the meaning list.
                    (helper (cdr symbol-dict-tail)
                            (cons (list symbol
                                        (find-inflec upper lower)) 
                                  meaning)))
                  (helper (cdr symbol-dict-tail) meaning)))))
      (helper (symbol-grammar grammar) '())))

  ;; Converts a message received from the speaking agent into a
  ;; meaning, and runs training against the meaning that this agent
  ;; received from the universe if this is a training round.
  (define (message->meaning message parameters)
    (let ((agent-to-speak (car parameters))
          (mode (cadr parameters)))
      (if (eq? agent-to-speak (parent-agent)) 
          silence
          (if (eq? mode 'training)
              (let ((meaning 
                     (get-last-event-in 
                       (get-history (parent-agent)))))
                (train message meaning)
                meaning)
              (tap (lookup message))))))

  ;; Make and return the language.
  (make-language grammar
                 meaning->message
                 message->meaning
                 update-grammar!
                 parent-agent))

;;; AGENT

;;; Feedback is handled by message->meaning
(define (basic-feedback-proc agent event rec-message interp params)
  'ok)

;;; Make and return the agent.
(define (make-beal-agent)
  (make-agent make-beal-language
              (lambda (x) x)
              (lambda (x) x)
              make-basic-history
              basic-feedback-proc))


;;; CHANNEL

;;; This function takes a message and a permutation and scrambles the
;;; message according to the permutation. 
(define (scramble perm message)
  (define scrambled (dict))
  (define (helper mess-tail)
    (if (eq? (car mess-tail) dict-end)
        scrambled
        (begin
          (dict-put! scrambled 
                     (list-ref perm (caar mess-tail))
                     (cadar mess-tail))
          (helper (cdr mess-tail)))))
  (helper message))


;;; The channel passes messages between agents.
;;; The agent to speak can perceive both agents' messages (but doesn't
;;; do feedback). The agent to listen can perceive the
;;; agent-to-speak's message. Messages are scrambled according to
;;; comm-line-perm to prevent agents from using order information.
;;; At the end, the messages are added to the history.

(define (channel-transmit channel parameters)
  (let* ((a1 (get-channel-agent1 channel))
         (a2 (get-channel-agent2 channel))
         (a1-message 
          (get-last-message-out 
            (get-history a1)))
         (a2-message
          (get-last-message-out
            (get-history a2)))
         (a1-scramble 
          (scramble comm-line-perm a1-message))
         (a2-scramble
          (scramble back-perm a2-message))
         (agent-to-speak (car parameters))
         (a1-transmitted
          (if (eq? a1 agent-to-speak)
              (comm-lines-merge a1-message a2-scramble)
              a2-scramble))
         (a2-transmitted
          (if (eq? a2 agent-to-speak)
              (comm-lines-merge a2-message a1-scramble)
              a1-scramble)))
    (set-last-message-in! 
      (get-history a2)
      channel
      a1-transmitted)
    (set-last-message-in!
      (get-history a1)
      channel
      a2-transmitted)))


;;; EXPERIMENTAL SETUP

(define number-of-training-runs 1200)
(define number-of-test-runs 5)
(define (make-beal-experiment)
  (let* ((agent-1 (make-beal-agent))
         (agent-2 (make-beal-agent))
         (channel (make-channel noiseless-channel agent-1 agent-2)))
    (make-experiment (list agent-1 agent-2)
                     (list channel)
                     sample-universe-event)))

#|
This is an example run with the settings as provided. Output is in the
form event event interpretation.
Events are printed by tap statements in meaning->message.
Other lines can be uncommented to print information about comm and
inflection mappings.
Interpretaions are printed by tap in message->meaning. 

1 ]=> ((make-beal-experiment))
 (((out place) (write verb) (oil object) (it subject)))
 (((out place) (write verb) (oil object) (it subject)))
 ((out place) (it subject) (oil object) (write verb))
 (((come verb) (they object) (that subject)))
 (((come verb) (they object) (that subject)))
 ((come verb) (they object) (that subject))
 (((go verb) (water object) (way subject)))
 (((go verb) (water object) (way subject)))
 ((go verb) (way subject) (water object))
 (((simple-present tense) (make verb) (part object) (she subject)))
 (((simple-present tense) (make verb) (part object) (she subject)))
 ((simple-present tense) (part object) (she subject) (make verb))
 (((past-perfect-simple tense) (affermative type) (find verb) (she object) (he subject)))
 (((past-perfect-simple tense) (affermative type) (find verb) (she object) (he subject)))
 ((past-perfect-simple tense) (find verb) (she object) (he subject) (affermative tense))
 ;Value 66: (ok ok ok ok ok)
|#