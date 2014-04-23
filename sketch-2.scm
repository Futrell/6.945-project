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

  (define respond-to-feedback! add-meaning-message-pair-to-grammar!)

  (define (add-meaning-message-pair-to-grammar! meaning message)
    (set! grammar (cons (list meaning message) grammar))
    (pp "new grammar:")
    (pp grammar))

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


;;; AGENT

(define (make-agent language
		    perceive-proc
		    interpret-proc)
  (list language perceive-proc interpret-proc))

(define (get-language agent)
  (list-ref agent 0))

(define (get-perceive-proc agent)
  (list-ref agent 1))

(define (get-interpret-proc agent)
  (list-ref agent 2))

(define (make-simple-agent)
  (make-agent (make-simple-language)
	      (lambda (x) x)
	      (lambda (x) x)))


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
			 channel 
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
	 (feedback-signals (get-feedback agents meanings decoded-meanings 
					 sent-messages received-messages
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

