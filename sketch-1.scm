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
		   ; two-agent, who will try to decode it
	 )

  (let* ((encoded-message (agent-encode alice semantics))
	 (decoded-message (agent-decode bob encoded-message))) 
    ;; this is starting to feel like OOP...
    decoded-message))

(define (run-communication-trial universe)
  (let ((from-agent (select-from-agent universe))
	(to-agent (select-to-agent universe))
	(semantics (select-semantics universe)))
    (let ((decoded-semantics
	   (communication-trial from-agent to-agent semantics)))
      (backchannel from-agent to-agent semantics decoded-semantics)
      'ok)))

;;; Question: How much state do we want to have? Should this function
;;; return new values for alice and bob or mutate them?  

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


