;;; Goal: Hack together a simple functional mock-up of the
;;; architecture. Agents that communicate two symbols, 'foo and 'bar. 
;;;
;;;
;;;          (Universe)
;;;         /          \
;;;        /            \
;;;       /              \
;;;  (Alice) - - - - - - (Bob)
;;;
;;; To model:
;;; Interfaces:
;;;    Universe -> Alice (universe representation of events -> Alice's
;;;                 representation of events) (Alice's perception
;;;                 function)
;;;
;;;    Alice -> Universe (Alice's representation of events ->
;;;                 Universe's representation of events) (Alice's
;;;                 interpretation function)
;;;
;;;    Alice -> Bob (Alice's encoding procedure)
;;;    Bob -> Alice (Alice's decoding procedure)
;;;    Optional: Feedback Alice -> Bob and/or Bob -> Alice. 
;;;    Optional: Feedback Universe -> Alice.
;;;
;;; Things:
;;;    Universe: Generates events.
;;;    Channel: The communication channel, introduces noise maybe.
;;;    Language: The grammar that the agents learn, which informs
;;;        their encoding and decoding methods. This includes update
;;;        mechanisms. 
;;;
;;; The universe interfaces are fixed. The task is to learn the
;;; communication interfaces. 


;;; UTILS

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

(define silence '(silence))
;;;

(define gensym generate-uninterned-symbol)

;;; let's do the simplest possible mockup
(define (sample-universe-event)
  (list-ref '(foo bar) (random 2)))


;;; LANGUAGE

;;; For now a language is just a mapping of semantic symbols to
;;; message symbols. A bidirectional association list for now.
(define (lookup-ref object alist n) ; n = 0 or 1
  (let ((assq-ref (association-procedure eq? 
                      (lambda (lst) (list-ref lst n)))))
    (let ((result (assq-ref object alist)))
      (if result
          (list-ref result (- 1 n)) ; 1->0, 0->1
          #f))))

(define (make-simple-language parent-agent) 
  (define grammar '())

  (define (add-meaning-message-pair-to-grammar! meaning message)
    (set! grammar (cons (list meaning message) grammar))
    (pp "new grammar:")
    (pp grammar))

  (define respond-to-feedback! add-meaning-message-pair-to-grammar!)

  (define (dont-know-message-for-meaning meaning)
    (let ((made-up-message (gensym)))
      (add-meaning-message-pair-to-grammar! meaning made-up-message)
      made-up-message))

  (define (dont-know-meaning-of-message message)
    'i-dont-know) ; give up

  (define (meaning->message meaning parameters) ; Make these "abstract methods"
    (let ((message (lookup-ref meaning grammar 0)) ; make this
          (agent-to-speak (car parameters)))
                                        ; grammar-lookup 
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

  (define (update-grammar! feedback) ; let's say feedback in this case
                                    ; is a signal during the training
                                    ; phase which says "the correct
                                    ; interpretation of the previous trial
                                    ; was x". Feedback needs to be an
                                    ; object that wraps up many
                                    ; things....
    (if (not (eq? feedback 'ok))
        (begin
          (apply respond-to-feedback! feedback))))


  (make-language grammar 
                 meaning->message 
                 message->meaning 
                 update-grammar!
                 parent-agent))

(define (make-language grammar 
                       meaning->message 
                       message->meaning
                       update-grammar!
                       parent-agent)
  (list grammar 
        meaning->message 
        message->meaning 
        update-grammar!
        parent-agent))

(define (get-grammar language)
  (list-ref language 0))

(define (get-encode-proc language)
  (list-ref language 1))

(define (get-decode-proc language)
  (list-ref language 2))

(define (get-update-grammar-proc language)
  (list-ref language 3))

(define (get-agent language)
  ((list-ref language 4)))



;;; HISTORY
;;; Used in AGENT to store past data which is used for feedback.
;;;    Note that conceptually history is more related to the universe,
;;;    in particular it may store signals that the agent would only
;;;    see through a noisy perceive-proc
;;; The basic history here is a list of:
;;; 0: The most recent event that the agent received from the universe
;;; 1: The meaning interpreted of that event
;;; 2: The message computed of that meaning
;;; 3: An alist of channels, and the most recent message the agent
;;;    received from the channel.
;;; 4: An alist of channels, and the meanings computed from the
;;;    messages.
;;; 5: An alist of channels, and the events interpreted from the
;;;    meanings.


(define (make-basic-history)
  (list '() '() '() (dict) (dict) (dict)))

(define get-last-event-in first)
(define get-last-e-meaning second)
(define get-last-message-out third)

(define (get-last-message-in history channel)
  (cadr (assq channel (fourth history))))
(define (get-last-m-meaning history channel)
  (cadr (assq channel (fifth history))))
(define (get-last-interp-out history channel)
  (cadr (assq channel (sixth history))))

(define (set-last-event-in! history new-ei) 
  (list-set-at! history 0 new-ei))
(define (set-last-e-meaning! history new-em)
  (list-set-at! history 1 new-em))
(define (set-last-message-out! history new-mo) 
  (list-set-at! history 2 new-mo))

(define (set-last-message-in! history channel new-mi)
  (dict-put! (fourth history) channel new-mi))
(define (set-last-m-meaning! history channel new-mm)
  (dict-put! (fifth history) channel new-mm))
(define (set-last-interp-out! history channel new-eo)
  (dict-put! (sixth history) channel new-eo))


;;; AGENT

;;; An agent is a list of:
;;; 0: a language (see above)
;;; 1: perceive-proc converts an event from the universe into a
;;;    meaning.
;;; 2: interpret-proc converts a meaning into a signal to the
;;;    universe. 
;;; 3: history (see above)

(define (make-agent language-maker
                    perceive-proc
                    interpret-proc
                    history-maker
                    feedback-proc)
  (let* ((agent 'not-defined-yet)
         (ret-agent (lambda () agent)) ;This setup lets us pass the
				       ;agent around so the language
				       ;can reference it.
         (agent-list (list (language-maker ret-agent)
                            perceive-proc 
                            interpret-proc 
                            (history-maker)
                            feedback-proc)))
    (set! agent agent-list)
    agent))

(define (get-language agent)
  (list-ref agent 0))

(define (get-perceive-proc agent)
  (list-ref agent 1))

(define (get-interpret-proc agent)
  (list-ref agent 2))

(define (get-history agent)
  (list-ref agent 3))

(define (get-feedback-proc agent)
  (list-ref agent 4))

(define (set-history! agent new-history)
  (list-set-at! agent 4 new-history))

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

(define (make-channel channel-model agent-1 agent-2)
  (list channel-model agent-1 agent-2))

(define get-channel-model first)
(define get-channel-agent1 second)
(define get-channel-agent2 third)
(define (get-other-agent channel agent)
  (cond 
    ((eq? agent (get-channel-agent1 channel))
     (get-channel-agent2 channel))
    ((eq? agent (get-channel-agent2 channel))
     (get-channel-agent1 channel))
    (else (error "No such agent for this channel")))) 

(define (reverse items)
  (fold-right (lambda (x r) (append r (list x))) '() items))

(define (transmit channel-model message)
  (channel-model message))

(define (noiseless-channel message) message)


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

(define (channel-give-feedback channel parameters)
  (let* ((agent-1 (get-channel-agent1 channel))
         (agent-2 (get-channel-agent2 channel))
         (fback-proc-1 (get-feedback-proc agent-1))
         (fback-proc-2 (get-feedback-proc agent-2))
         (event-1 (get-last-event-in (get-history agent-1)))
         (event-2 (get-last-event-in (get-history agent-2)))
         (m-in-1 (get-last-message-in (get-history agent-1) channel))
         (m-in-2 (get-last-message-in (get-history agent-2) channel))
         (interp-1 (get-last-interp-out (get-history agent-1)
					channel))
         (interp-2 (get-last-interp-out (get-history agent-2)
					channel))
         (feedback-1 
          (fback-proc-1 agent-1 event-2 m-in-1 interp-1 parameters))
         (feedback-2
          (fback-proc-2 agent-2 event-1 m-in-2 interp-2 parameters))
         (update-1 (get-update-grammar-proc (get-language agent-1)))
         (update-2 (get-update-grammar-proc (get-language agent-2))))
    (update-1 feedback-1)
    (update-2 feedback-2)))


;;; EXPERIMENTAL SETUP
          
(define number-of-training-runs 5)
(define number-of-test-runs 5)

(define (do-n-times n thunk)
  (define (iterate i)
    (if (< i n)
        (begin (thunk) 
               (iterate (+ i 1)))))
  (iterate 0))

(define (make-experiment agents 
                         channels
                         sample-universe-event)
  (define clock 0)

  (define (run-cycle parameters)
    (let* ((event (sample-universe-event))
           (agent-to-speak (list-ref agents (random (length agents))))
           (parameters (list agent-to-speak parameters)))
      (one-full-cycle agents event channels parameters clock)))
  
  (define (run)
    (do-n-times number-of-training-runs
                (lambda ()
                  (run-cycle 'training)))
    (do-n-times number-of-test-runs
                (lambda ()
                  (run-cycle 'testing))))
  run)

(define (make-simple-experiment)
  (let* ((agent-1 (make-simple-agent))
         (agent-2 (make-simple-agent))
         (channel (make-channel noiseless-channel agent-1 agent-2)))
    (make-experiment (list agent-1 agent-2)
                     (list channel)
                     sample-universe-event)))

(define (tap x)
  (pp x)
  x)

(define (one-full-cycle
         agents
         event
         channels
         parameters
         clock)
  (set! clock (+ clock 1))
  (agents-perceive agents event parameters)
  (agents-encode agents parameters)
  (map channel-transmit channels)
  (agents-decode channels parameters)
  (agents-interpret channels parameters)
  (do-feedback channels parameters) ; what is
                                        ; the best way to specify how
                                        ; feedback works? it doesn't
                                        ; seem right to pass in all
                                        ; this stuff...
  'ok)


(define (agents-perceive agents event parameters)
  (let ((meanings 
        (map (lambda (agent) ((get-perceive-proc agent) event))
             agents)))
    (map (lambda (agent meaning)
           (set-last-event-in! (get-history agent) event)
           (set-last-e-meaning! (get-history agent) meaning))
         agents
         meanings)))

(define (agents-encode agents parameters)
  (letrec ((messages 
           (map (lambda (agent)
                  ((get-encode-proc (get-language agent)) 
                   (get-last-e-meaning (get-history agent))
                   parameters))
                agents)))
    (map (lambda (agent message) 
           (set-last-message-out! (get-history agent) message))
         agents
         messages)))

(define (channel-decode-all channel parameters)
  (let* ((agent-1 (get-channel-agent1 channel))
         (agent-2 (get-channel-agent2 channel))
         (meaning-1 ((get-decode-proc (get-language agent-1))
                     (get-last-message-in (get-history agent-1) 
                                          channel)
                     parameters))
         (meaning-2 ((get-decode-proc (get-language agent-2))
                     (get-last-message-in (get-history agent-2)
                                          channel)
                     parameters)))
    (set-last-m-meaning! (get-history agent-1) channel meaning-1)
    (set-last-m-meaning! (get-history agent-2) channel meaning-2))) 

(define (agents-decode channels parameters)
  (map (lambda (channel)
         (channel-decode-all channel parameters))
       channels))

(define (channel-interpret-all channel parameters)
  (let* ((agent-1 (get-channel-agent1 channel))
         (agent-2 (get-channel-agent2 channel))
         (event-1 ((get-interpret-proc agent-1)
                   (get-last-m-meaning (get-history agent-1) 
                                       channel)))
         (event-2 ((get-interpret-proc agent-2)
                   (get-last-m-meaning (get-history agent-2)
                                       channel))))
    (set-last-interp-out! (get-history agent-1) channel event-1)
    (set-last-interp-out! (get-history agent-2) channel event-2))) 

(define (agents-interpret channels parameters)
  (map (lambda (channel)
         (channel-interpret-all channel parameters))
       channels))


;;; this is tricky: we need to be able to specify how feedback works
;;; parametrically, but there are many kinds of possible feedback!
;;; Maybe we should just make the user supply this function?
;;;
;;; Anyway, for this simple mockup, let's assume that feedback works
;;; this way: In training, if an agent gets the decoded-meaning
;;; 'i-dont-know or gets it wrong, the universe will tell him what was
;;; intended. (In this simple setup, an agent should never get
;;; anything wrong.)
(define (do-feedback channels parameters)
  (map (lambda (channel)
         (channel-give-feedback channel parameters))
       channels))

;;; Two functions: one doing the mapping here; another calculating
;;; what the feedback is, along these dimensions:

;;;                correction     right/wrong
;;; no reason
;;; reason
;;; 

;;; Refactor Each AGENT asks for a kind of feedback. XXXX
;;; Generalize the language interface XX
;;; Names: Write a document to standardize. 


;;; I think that it would make sense to have a couple levels of
;;; complexity (sort of thinking along the lines of the Making Magic
;;; article Robert cited). 
;;; In particular, there are some elements of the agents that most
;;; people won't want to change, so we can have them more strongly
;;; built-in (i.e. most programmers don't see them).
;;; The main case that I'm thinking of is only having agents remember
;;; one time-step of the past. If someone really wants more they can
;;; alter the agent code or the way 'grammar' works, but otherwise
;;; there's just one available.

