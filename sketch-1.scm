;;; Here's what happens in a communication trial between Alice and
;;; Bob. The universe selects some event and sends it to Alice. Alice
;;; encodes the message with her grammar and sends it over the channel
;;; to Bob. Bob decodes the message with his grammar. We check whether
;;; the event semantics that Bob arrives at == the semantics that
;;; Alice was exposed to. 

;;; Different agents might represent the underlying semantics of
;;; events differently. In practice we will probably use the same
;;; representation. But we want to design this thing such that the
;;; agents can be heterogenous in this way. 

;;; this is how we decomposed the agents in the proposal
(define (make-agent world-knowledge grammar-knowledge
		    encode-proc decode-proc
		    update-grammar-proc)
  'unimplemented)

;;; Maybe the encoding, decoding, and update-grammar-proc should be
;;; properties of a grammar/language object that each agent has,
;;; rather than being separate properties of the agent...

;;; Anyway, a basic communication trial will look like this:

(define (communication-trial 
	 from-agent to-agent 
	 semantics ; this is the universe's representation of events,
		   ; which the from-agent will receive in its own way,
		   ; encode using the grammar, and send to the
		   ; to-agent, who will try to decode it
	 )

  (let* ((encoded-message (agent-encode alice semantics))
	 (decoded-semantics (agent-decode bob encoded-message)))
    ;; is message passing style the way to go here?
    decoded-semantics))

(define (run-communication-trial universe)
  (let ((from-agent (select-from-agent universe))
	(to-agent (select-to-agent universe))
	(semantics (select-semantics universe)))
    (let ((decoded-semantics
	   (communication-trial from-agent to-agent semantics)))
      (backchannel from-agent to-agent semantics decoded-semantics)
      'ok)))

;;; J: I think we want to be more general than this. 
;;; Beal's system is conceptually more like both agents speaking at
;;; once, with one agent being chosen to be successful (I think it
;;; has to do with how he models the brain?). 
;;; I think we could do something like this (*might be overly OOP*):
(define (one-full-cycle
         ordered-agent-set
         semantics ; as above
         comm-channel ; this is the channel between agents
                      ; it handles adding noise and passing messages
         settings ; ex. training vs. communicating
         )
  (set! clock (+ clock 1))
  (agents-read ordered-agent-set semantics settings)
    ;; typically agents read streams.
    ;; whether each agent reads every time, every other, when they
    ;; need to, etc. depends on implementation
    ;; conceptually, what an agent 'hears' may not be controlled by
    ;; any other part of the system
  (transmit comm-channel ordered-agent-set settings)
    ;; comm-channel calls agent-encode as above, does whatever
    ;; processing it does, then passes filtered messages to agents
  (agents-process ordered-agent-set settings)
    ;; agents all call agent-decode on their internal state,
    ;; which might have a recent message or an indicator from
    ;; comm-channel telling them to do nothing
    ;; agents may update internal state
  (agents-output ordered-agent-set settings) ; probably just prints
  (universe-update settings semantics clock)
)
;;; If this is too OOP we could have each step call the next.

;;; Question: How much state do we want to have? Should this function
;;; return new values for alice and bob or mutate them?  

    ;;; J: I think that given what we are modelling (states in the
    ;;; brain, physical devices swarms) mutation is more intuitive.
    ;;; It also depends on how we implement alice and bob.
    ;;; I guess they are probably tagged lists? Do we want agents
    ;;; to drag along functions, or do we use generic operators
    ;;; that depend on which agent-tag is present?

;;; What is becoming clear to me is that each agent needs two
;;; different interfaces: (1) with the universe and (2) with the other
;;; agents. The interface with the universe is given; the interface
;;; with other agents is learned. 

;;; (universe event) --(alice's perception interface)--> (alice's representation of the event)
;;; This also runs backward:
;;; (alice's rep) --(alice's perception interface)--> (universe event)
;;; 
;;; Within Alice we also have:
;;; (alice's rep) --(the grammar's encoding proc)--> (message)
;;; (message) --(the grammar's decoding proc)--> (alice's rep)
;;;
;;; The fact that each of these interfaces needs to be bidirectional
;;; suggests we could use propagators as one implementation. 
;;;
    ;;; J: Good idea. I think it would work event if agents have
    ;;; different grammars.

;;; The other big thing the agents need is a way to update their
;;; languages. Maybe that could be wrapped up into the language object
;;; too.
;;;
;;; So really agents should look like this:

(define (make-agent world-interface language)
  'unimplemented)

;;; And world-interface could look like this:

(define (make-world-interface perceive-proc interpret-proc)
  'unimplemented)

(define (perceive-proc universe-semantics)
  'agent-representation-placeholder)

(define (interpret-proc agent-representation)
  'universe-semantics-placeholder)

;;; language would look like this:

(define (language encode-proc decode-proc update-proc)
  ;; lots of stuff here to set up the grammatical inference algorithms
  'unimplemented)

;;; Or, alternatively, both the world-interface and the language could
;;; be propagator networks. In that case we could provide alternative
;;; constructors, or we could force the user to make encoding and
;;; decoding procedures to interface with that network. 

    ;;; J: I think that the languages should at least be separate 
    ;;; instances, so agents could have different languages. 
    ;;; Whether we move the difference in implementations into
    ;;; agent or language should depend on what's easier to code. I
    ;;; don't think it makes much conceptual difference.
