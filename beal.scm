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

(define initial-comm-lines-per-agent-symbl 600)
(define prob-initial-comm-line
  (/ (+ 0.0 initial-comm-lines-per-agent-symbl)
     (+ 0.0 num-comm-lines)))
(define closeness-allowed 6) ;how many comm lines off is a match

;;; Like dict-put!, but sets conflicting comm line values to 'X, with
;;; the exception of 0 which can be overwritten
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

;;; merge two dictionaries of comm-lines and their values
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

;;; associate values with comm lines, choosing 1 with probability
;;; inflection-prob and choosing -1 otherwise
(define (comm-lines-choose-vals lines inflection-prob)
  (define line-val-set (dict))
  (define (helper line-set)
    (if (null? line-set)
        line-val-set
        (let ((to-add (car line-set)))
          (if (wcoin inflection-prob)
              (comm-lines-put! line-val-set to-add 1)
              (comm-lines-put! line-val-set to-add -1))
          (helper (cdr line-set)))))
  (helper lines))

;;; generate a random set of comm lines (list of integers) by choosing
;;; each possible comm line with probability prob-initial-comm-line
(define (random-comm-set)
  (define (helper line-set i)
    (if (= i 0)
      line-set
      (if (wcoin prob-initial-comm-line)
          (helper (cons (- i 1) line-set) (- i 1))
          (helper line-set (- i 1)))))
  (helper '() num-comm-lines))

;;; intersect two sets of comm lines (list of integers)
(define (comm-sets-intersect c1 c2)
  (define (helper c1-sublst retlst)
    (if (null? c1-sublst)
        retlst
        (if (member (car c1-sublst) c2)
            (helper (cdr c1-sublst) (cons (car c1-sublst) retlst))
            (helper (cdr c1-sublst) retlst))))
  (helper c1 '()))

;;; removes the values from a dictionary of comm lines to values
;;; returns a list of integers
(define (extract-lines message)
  (if (eq? (car message) dict-end) ;not worrying about efficiency
      '()
      (cons (caar message) (extract-lines (cdr message)))))

;;; Counts numbers of 1s and Xs in a set of lines within a message
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

;;; (symbol, inflection)

(define (make-beal-language parent-agent) 
  (define grammar (list (dict) (dict)))
  (define symbol-grammar first)
  (define inflec-grammar second)
  ;; The grammar is a list of two dictionaries. 
  ;;   The first dictionary describes the observed symbols.
  ;;     Keys are observed symbols.
  ;;     Values are lists of certain lines.
  ;;   The second dictionary describes the observed inflections.
  ;;     Keys are observed inflections.
  ;;     Values are real number estimates of the inflection mappings.

;Beal drives lines with 1s and -1s, and the percentage of 1s is the
;inflection mapping. I guess for multiple items conflicts are marked
;as conflicts, and provide a range of values? If we have 6
;inflections, we want around 18-30 lines per symbol to accurately see
;them. That's 0.0018 in intersection, so about 500-600 lines
;originally for each. 

  (define (update-grammar! feedback)
    'ok)

  (define (lookup-sym-or-new-meaning symbol)
    (let ((sym-mess (assq symbol (symbol-grammar grammar))))
      (if (not sym-mess)
          (begin
            (dict-put! (symbol-grammar grammar) 
                       symbol 
                       (random-comm-set))
            (cadr (assq symbol (symbol-grammar grammar))))
          (cadr sym-mess))))

  (define (lookup-inf-or-new-meaning inflec)
    (let ((inf-mess (assq inflec (inflec-grammar grammar))))
      (if (not inf-mess)
          (begin
            (dict-put! (inflec-grammar grammar) 
                       inflec
                       (random 1.0))
            (cadr (assq inflec (inflec-grammar grammar))))
          (cadr inf-mess))))

  (define (meaning->message meaning parameters)
    (define message (dict))
(if (eq? (cadr parameters) 'testing)
(tap meaning))
    (define (helper meaning-lst)
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
            (else message))))

  (define (train message meaning)
    (let* ((symbol (caar meaning))
           (inflec (cadar meaning))
           (comm-lines (extract-lines message))
           (expected-lines (lookup-sym-or-new-meaning symbol))
           (matching-lines 
            (comm-sets-intersect comm-lines expected-lines))
           (expected-inf-prob (lookup-inf-or-new-meaning inflec)))
      (if (null? matching-lines)
          (dict-put! (symbol-grammar grammar)
                     symbol
                     (random-comm-set))
          (let* ((ones-xs (count-1s-xs matching-lines message))
                 (ones (car ones-xs))
                 (xs (cdr ones-xs))
                 (total (length matching-lines))
                 (lower (/ (+ ones 0.0) (+ total 0.0)))
                 (upper (/ (+ ones xs 0.0) (+ total 0.0))))
            (dict-put! (symbol-grammar grammar)
                       symbol
                       matching-lines)
            (cond
              ((and (<= lower expected-inf-prob) 
                    (>= upper expected-inf-prob))
               'ok) ; expected-inf-prob is in the right range
              ((> lower expected-inf-prob)
               (dict-put! (inflec-grammar grammar) inflec lower))
              (else 
               (dict-put! (inflec-grammar grammar) inflec upper)))))
      (if (not (null? (cdr meaning)))
          (train message (cdr meaning)))))

  (define (find-inflec upper lower)
    (define (helper inflec inf-dict)
      (if (eq? (car inf-dict) dict-end)
          (if inflec
              inflec
              'no-inflection-found)
          (if (and (<= lower (cadar inf-dict)) 
                   (>= upper (cadar inf-dict)))
              (if inflec
                  'multiple-inflections
                  (helper (caar inf-dict) (cdr inf-dict)))
              (helper inflec (cdr inf-dict)))))
    (helper #f (inflec-grammar grammar)))

  (define (lookup message)
    (let* ((mess-lines (extract-lines message)))
      (define (helper symbol-dict-tail meaning)
        (if (eq? (car symbol-dict-tail) dict-end)
            meaning
            (let* ((symbol (caar symbol-dict-tail))
                   (symbol-lines (cadar symbol-dict-tail))
                   (matching-lines 
                    (comm-sets-intersect mess-lines symbol-lines)))
              (if (< (abs (- (length symbol-lines) 
                             (length matching-lines)))
                     closeness-allowed)
                  (let* ((ones-xs (count-1s-xs matching-lines message))
                         (ones (car ones-xs))
                         (xs (cdr ones-xs))
                         (total (length matching-lines))
                         (lower (/ (+ ones 0.0) (+ total 0.0)))
                         (upper (/ (+ ones xs 0.0) (+ total 0.0))))
                    (helper (cdr symbol-dict-tail) 
                            (cons (list symbol
                                        (find-inflec upper lower)) 
                                  meaning)))
                  (helper (cdr symbol-dict-tail) meaning)))))
      (helper (symbol-grammar grammar) '())))

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
            



  (make-language grammar
                 meaning->message
                 message->meaning
                 update-grammar!
                 parent-agent))

;;; AGENT

;Feedback is handled by message->meaning
(define (basic-feedback-proc agent event rec-message interp params)
  'ok)

(define (make-beal-agent)
  (make-agent make-beal-language
              (lambda (x) x)
              (lambda (x) x)
              make-basic-history
              basic-feedback-proc))


;;; CHANNEL

(define (scramble perm message)
  (define scrambled (dict))
  (define (helper mess-tail) ; This could be replaced with map
    (if (eq? (car mess-tail) dict-end)
        scrambled
        (begin
          (dict-put! scrambled 
                     (list-ref perm (caar mess-tail))
                     (cadar mess-tail))
          (helper (cdr mess-tail)))))
  (helper message))

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

(define number-of-training-runs 1000)
(define number-of-test-runs 5)

(define (make-beal-experiment)
  (let* ((agent-1 (make-beal-agent))
         (agent-2 (make-beal-agent))
         (channel (make-channel noiseless-channel agent-1 agent-2)))
    (make-experiment (list agent-1 agent-2)
                     (list channel)
                     sample-universe-event)))

