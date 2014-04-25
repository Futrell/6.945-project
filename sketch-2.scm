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

(define (make-simple-language) 
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

  (define (meaning->message meaning) ; Make these "abstract methods"
    (let ((message (lookup-ref meaning grammar 0))) ; make this
                                        ; grammar-lookup 
      (if message
	  message
	  (dont-know-message-for-meaning meaning))))

  (define (message->meaning message)
    (let ((meaning (lookup-ref message grammar 1)))
      (if meaning
	  meaning
	  (dont-know-meaning-of-message message))))

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
		 update-grammar!))

(define (make-language grammar 
		       meaning->message 
		       message->meaning
		       update-grammar!)
  (list grammar 
	meaning->message 
	message->meaning 
	update-grammar!))

(define (get-grammar language)
  (list-ref language 0))

(define (get-encode-proc language)
  (list-ref language 1))

(define (get-decode-proc language)
  (list-ref language 2))

(define (get-update-grammar-proc language)
  (list-ref language 3))

(define (set-grammar! language new-grammar)
  (list-set-at! language 0 new-grammar))

(define (set-encode-proc! language new-encode-proc)
  (list-set-at! language 1 new-encode-proc))

(define (set-decode-proc! language new-decode-proc)
  (list-set-at! language 2 new-decode-proc))

(define (set-update-grammar-proc! language new-update-grammar-proc)
  (list-set-at! language 3 new-update-grammar-proc))


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

(define (get-last-event-in history) first)
(define (get-last-e-meaning history) second)
(define (get-last-message-out history) third)

(define (get-last-message-in history channel)
  (cadr (assq (fourth history) channel)))
(define (get-last-m-meaning history channel)
  (cadr (assq (fifth history) channel)))
(define (get-last-event-out history channel)
  (cadr (assq (sixth history) channel)))

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
(define (set-last-event-out! history channel new-eo)
  (dict-put! (sixth history) channel new-eo))


;;; AGENT

;;; An agent is a list of:
;;; 0: a language (see above)
;;; 1: perceive-proc converts an event from the universe into a
;;;    meaning.
;;; 2: interpret-proc converts a meaning into a signal to the
;;;    universe. 
;;; 3: history (see above)

(define (make-agent language
		    perceive-proc
		    interpret-proc
                    history)
  (list language perceive-proc interpret-proc history))

(define (get-language agent)
  (list-ref agent 0))

(define (get-perceive-proc agent)
  (list-ref agent 1))

(define (get-interpret-proc agent) ;; Need a better name here
                                   ;; Conceptually, perceive backwards
  (list-ref agent 2))

(define (get-history agent)
  (list-ref agent 3))

(define (set-history! agent new-history)
  (list-set-at! agent 4 new-history))

(define (make-simple-agent)
  (make-agent (make-simple-language)
	      (lambda (x) x)
	      (lambda (x) x)
              (make-basic-history)))


;;; CHANNEL

(define (make-channel channel-model agent-1 agent-2)
  (list channel-model agent-1 agent-2))

(define (get-channel-model) first)
(define (get-channel-agent1) second)
(define (get-channel-agent2) third)

(define (channel-transmit)
  )

(define (channel-give-feedback channel parameters)
  )


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
			 channel-model 
			 sample-universe-event)
  (define clock 0)

  (define (run-cycle parameters)
    (let* ((event (sample-universe-event))
	   (agent-to-speak (random (length agents)))
	   (parameters (list agent-to-speak parameters)))
      (one-full-cycle agents event channel parameters clock)))
  
  (define (run)
    (do-n-times number-of-training-runs
		(lambda ()
		  (run-cycle 'training)))
    (do-n-times number-of-test-runs
		(lambda ()
		  (run-cycle 'testing))))
  run)

(define (make-simple-experiment)
  (make-experiment (list (make-simple-agent)
			 (make-simple-agent))
		   noiseless-channel
		   sample-universe-event))

(define (reverse items)
  (fold-right (lambda (x r) (append r (list x))) '() items))

(define (transmit messages channel)
  (reverse (map channel messages)))

(define (noiseless-channel message) message)

(define (tap x)
  (pp x)
  x)

(define (one-full-cycle
	 agents
	 event
	 channel
	 parameters
	 clock)
  (set! clock (+ clock 1))
  (let* ((meanings (agents-perceive agents event parameters))
	 (sent-messages (agents-encode agents meanings parameters))
	 (received-messages (transmit sent-messages channel))
	 (decoded-meanings (agents-decode agents received-messages 
					  parameters))
	 (interpretations (agents-interpret agents decoded-meanings 
					    parameters))
	 (feedback-signals (get-feedback agents received-messages
					 event interpretations
					 parameters))) ; what is
					; the best way to specify how
					; feedback works? it doesn't
					; seem right to pass in all
					; this stuff...
    (update-agents! agents feedback-signals)
    'ok
    ))

(define (update-agents! agents feedback-signals)
  (for-each (lambda (agent feedback-signal) 
	      (let ((update-proc! (get-update-grammar-proc
				  (get-language agent))))
		(update-proc! feedback-signal)))
	    agents feedback-signals))

(define (agents-perceive agents event parameters)
  (map (lambda (agent) ((get-perceive-proc agent) event))
       agents))

(define (agents-encode agents meanings parameters)
  (let ((agent-to-speak (list-ref agents (car parameters))))
    (map (lambda (agent meaning)
	   (if (eq? agent agent-to-speak)
	       ((get-encode-proc (get-language agent)) meaning)
	       'silence))
	 agents meanings)))

(define (agents-decode agents messages parameters)
  (let ((agent-to-listen (list-ref agents (- 1 (car parameters)))))
    (map (lambda (agent message) 
	   (if (eq? agent agent-to-listen)
	       ((get-decode-proc (get-language agent)) message)
	       'silence))
	   agents messages)))

(define (agents-interpret agents meanings parameters)
  (map (lambda (agent meaning) 
	 (if (not (eq? meaning 'silence))
	     ((get-interpret-proc agent) meaning)
	     'silence))
       agents meanings))

;;; this is tricky: we need to be able to specify how feedback works
;;; parametrically, but there are many kinds of possible feedback!
;;; Maybe we should just make the user supply this function?
;;;
;;; Anyway, for this simple mockup, let's assume that feedback works
;;; this way: In training, if an agent gets the decoded-meaning
;;; 'i-dont-know or gets it wrong, the universe will tell him what was
;;; intended. (In this simple setup, an agent should never get
;;; anything wrong.)
(define (get-feedback agents received-messages
		      event interpretations
		      parameters)
  (map (lambda (agent received-message interpretation)
	 (if (and (eq? (cadr parameters) 'training)
		  (not (eq? interpretation 'silence))
		  (not (eq? interpretation event)))
	     (list ((get-perceive-proc agent) event) received-message)
	     'ok))
       agents received-messages interpretations))

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