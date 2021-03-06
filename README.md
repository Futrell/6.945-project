A Library for/of Communicating Machines
=============

Project for 6.945, Spring 2014
James Duyck and Richard Futrell

We want to create a highly modular framework in which to implement and experiment with various models of machines that rationally communicate with each other, or which learn to communicate.

Experiment Harness
------------------

We want to create an experimental harness that allows agents to be specified in a highly modular way. What we made is in `experiment.scm`.

The experimental setup is as follows. There is a **universe** which generates **events** (the denotations of messages to be encoded). There are two agents. In an experimental trial, one or both agents **perceive** an **event**, producing an internal representation of the event. Then an agent **encodes** the event into a message, it passes through the channel to the other agent, and that agent **decodes** it into an internal representation. That internal representation is then **interpreted**, producing an event which the universe can then evaluate for feedback purposes.

So there are two basic kinds of bidirectional interfaces:

1.  Universe <-> Agent. The agent `perceives` events from the universe (event -> agent's internal representation), and `interprets` its internal event representations when sending them to the universe for feedback (agent's internal representation -> event). This interface is fixed.
2.  Agent <-> Agent. The agents encode messages (agent's internal representation -> message) and decode messages (message -> agent's internal representation). This interface is to be learned.

An *agent* consists of: 

1.  A language (more on that soon),
2.  `perceive-proc` and `interpret-proc`: These comprise the interface with the universe.
3.  History: An agent's memory for previous communication trials in an experiment.
4.  `feedback-proc`: A feedback procedure which might mutate the language.

A *language* is fundamentally a pair of procedures `meaning->message` and `message->meaning`. For the purposes of carrying out experiment, an experimenter might want to specify a few more parts. So in the experiment harness, a language is parameterized by:

1.  A grammar, which can be anything. If the procedures `meaning->message` and `message->meaning` involve any shared body of knowledge, then this object references that, and allows it to be accessed and updated.
2.  A procedure `meaning->message`
3.  A procedure `message->meaning`
4.  A procedure `update-grammar!` which will be called by `feedback-proc`.
5.  `parent-agent`: A thunk of the agent that has this language. 

In the interests of flexibility, we decided that the steps in each experimental trial (e.g. perceiving events, encoding messages, ...) occur by mutation of variables such as `last-message-in`. Using state here has a major drawback in terms of implementation: it limits the interoperability of our code with probabilistic programming languages such as Church which are pure. 

However, we originally tried to implement the experimental trials as statelessly as possible, and found it quite cumbersome. You can see fragments of this attempt in `sketch-1.scm`. In particular, the feedback procedure was troublesome. It might require the entire history of variables generated during a trial and in previous trials; the exact form of the feedback function and what information it needs should be specified by the experimentor. When trials proceed statelessly, this means that **all** the intermediate variables generated need to be passed into the feedback function, forcing the experimentor to write a feedback function with a very large number of arguments, most of which will probably be thrown away. In general, rather than make potentially limiting design decisions about what information would go exactly where, it seemed simpler to us to  have have each trial develop various state variables and allow the experimentor to access those variables.

![Data flow through system](http://web.mit.edu/~jduyck/www/dfd.png.png)

This diagram shows the main data structures of the experiment harness. Note the **history** data structure, which holds state information. Also note that each agent may have connections to multiple channels.


Beal Model
----------

We based much of the experiment harness around the framework used by Beal (2001, 2002). The experiment harness contains a generalized version of the parts of Beal's framework that we thought were not unique to Beal's experiments. `beal.scm` contains elements of Beal's framework that are specific to the problem that he studied. We have simplified some elements of Beal's method.

In Beal's model, there are two agents. Each agent has a set of **feature lines** and the agents are connected by **comm lines**. 

Events generated by the universe consist of a set of **symbols** and **inflections**. The basis for this is in language: for example, the sentence "The dog sees the cat," might be represented by the symbols dog, see, cat and the corresponding inflections subject, verb, object. In general, symbols and inflections may not be words, but any sets where there are many symbols and few inflections. When the universe generates an event, it drives a feature line for each symbol with the corresponding inflection (represented in code by an ordered pair). Note that agents are not aware of the sets of possible symbols and inflections - agents' languages add symbols and inflections to their grammars as they encounter them.

The agents are connected by set of comm lines. When an agent sends a message, each comm line is set to one of the values 0 (undriven), 1, -1, or X. These are managed by the communication channel. The lines are scrambled to prevent agents from learning languages using ordering rather than feedback.

A speaking agent encodes an event to a message as follows. For each symbol the agent maintains a list of comm lines, the **comm mapping**, and for each inflection the agent maintains a real number, the **inflection mapping**. The inflection mappings are floats between 0 and 1. Comm mappings and inflection mappings are initially randomly generated.

For each symbol-inflection pair in the event, if the inflection mapping is p and there are l lines in the comm mapping, then pl random lines are driven to 1 and the rest to -1. If two symbols try to set a line differently, the line is set to X. 

A listening agent interprets the message by comparing the driven lines to its own comm mappings. If most of the lines corresponding to a symbol are driven, then that symbol is assumed to be part of the message. The agent then calculates the fraction of those lines that are driven to 1 (or X), and finds the closest inflection mapping to determine the inflection of the symbol.

Agents learn each other's symbol and inflection mappings through a training stage. During training, both agents receive the same event. When an agent receives a message, it compares the the set of comm lines driven to the comm mappings for each symbol in the event. It sets the comm mapping for each symbol to the intersection of that comm mapping and the set of driven lines. This removes lines that one agent maps to a symbol but which the other doesn't. Since symbols are encoded in groups, an agent must pare down each comm mapping multiple times with other groups of symbols to ensure that it has the same set of lines as the other agent.

Agents learn inflection mappings by looking at the fraction of 1s (and Xs) in the lines of a message corresponding to a symbol. This is compared with the agent's inflection mapping for that symbol. If the fraction is close to the inflection mapping, the inflection mapping is assumed to be correct. If the fraction is somewhat close, the inflection mapping is moved closer to the fraction. Otherwise, the inflection mapping is randomly re-chosen.

The code in `beal.scm` can make agents that accurately learn symbol matchings using symbols chosen from the included lists of words. Agents can learn inflection mappings, but are often inaccurate. Future work includes improving performance on learning inflection mappings. Additional avenues that could be explored involve using more explicitly probabilistic implementations of Beal's code and attempting to translate between languages with different sets of symbols and inflections. Another possible avenue of exploration is to implement the communication channel using propagators - this is a natural choice because it accurately models the movement of information in the types of systems that Beal examines.


Smith, Frank & Goodman Model
-----------

We originally intended to implement the agents in Smith, Goodman & Frank (2013), which learn a lexicon within a framework of recursive reasoning about other agents. We haven't gotten to the learning part yet, but did implement the recursively reasoning agents, which were introduced in Frank & Goodman (2012). 

There are two ways agents could communicate: first, **literally**: they formulate and interpret messages by looking them up in a dictionary or grammar. Second, **pragmatically**: they reason about what message will maximize their likelihood of communicative success, and about what messages would be sent by another agent doing such reasoning. The Smith, Goodman & Frank models attempt to formalize the latter kind of communicating agent.

For example, suppose Alice wants to communicate one of these three entities to Bob:

![Three entities](http://web.mit.edu/futrell/www/faces.png)

Suppose she sends the message "the faces with glasses". In fact, in experiments, this is often the message that people choose to send to indicate the second face (the one with ONLY glasses). Now if Bob's language interpretation procedure is just to look up the meanings in a lookup table of some kind, he'll be in trouble: there are two faces here that have glasses. 

To interpret the message, Bob should think: if Alice had meant the guy with the glasses and the hat, she would have mentioned the hat, since that would uniquely specify the referent. But she didn't, so she must mean the guy with ONLY glasses.

We can formalize this kind of reasoning using Bayesian inference. A listener L tries to infer the most likely meaning M that would have led to the utterance U from a speaker S. That is, L computes the posterior distribution over meanings M given that a speaker S said U:

P(meaning is M | S says U) = p(S says U | meaning is M) p(meaning is M)

 Similarly, a speaker S finds the utterance U that maximizes the probability that a listener L will decode the meaning M. We can formalize that by having the speaker S sample an utterance U from (the softmax of) a listener L's the posterior distribution of meanings given U, conditioned on the meaning being M:

p(message is U | meaning is M) = softmax of P(L thinks meaning is M | message is U)

So in interpreting and formulating utterances, agents S and L have to reason about the behavior of other agents S' and L', which might themselves be reasoning about other agents S'' and L''. This recursive reasoning can then bottom out in two possible base cases: L<sub>literal</sub>, a literal listener who interprets utterances by looking them up in a lexicon, or S<sub>literal</sub>, a speaker who looks up utterances in a table. 

Crucially for our framework, a pragmatically reasoning speaker can work just by sampling from the distribution of utterances from any listener, whether that listener be pragmatic or literal. 

`smith-interface.scm` contains an interface for building **speakers** (`meaning->message` procedures) and **listeners** (`message->meaning` procedures) of the literal and pragmatic types. These procedures can then be used to make agents in the experiment framework.

`smith-interface.scm` defines constructors for literal listeners and literal speakers that look up utterances in an alist (their lexicon). There are constructors for pragmatic listeners: these are constructed by passing in a speaker, which the pragmatic listener will reason about. And there is a constructor for pragmatic speakers: these are constructed by passing in a listener *and a speaker* (there are subtle reasons why this is necessary: to find out, consult the commentary in `smith-interface.scm`). We also provide constructors for "language interfaces", which are just speaker-listener pairs. 

Here is how we can build an agent that listens by reasoning about a literal speaker, and that speaks by reasoning about a pragmatic listener:

    (make-pragmatic-language-interface
      (make-literal-listener-language-interface lexicon))

Here's the cool part: the pragmatic speakers and listeners are built on top of any other listeners and speakers. So it is possible that the base case of the recursion is not just a `literal-listener` as defined in `smith-interface`, but rather something more complex like a Beal agent. Or the pragmatic language interfaces can sit on top of other pragmatic langauge interfaces:

    (make-pragmatic-language-interface
     (make-pragmatic-language-interface
      (make-pragmatic-language-interface
       (make-pragmatic-language-interface
        (make-pragmatic-language-interface
         (make-literal-listener-language-interface lexicon))))))

We provide two implementations of the Smith interface. The first is written in Church, a probabilistic variant of Scheme. This is `simple-pragmatics.church`. Since the existing Church implementations are quite slow, this has not been tested with deeply nested agents; but it is useful as a conceptual model.

The second implementation uses `amb` to implement inference by rejection sampling. The mechanism here is to generate many samples of utterances or meanings randomly, then do conditioning using `amb`'s `require` mechanism. This implementation is tested against two universes. The first is found in `simple-pragmatics-test.scm`. This is a friendly universe with no ambiguity. The second is `simple-pragmatics-test-ambiguous.scm`, which is the universe with the three faces as in the figure above. In this universe, the `amb`-pragmatic agents end up producing fewer ambiguous utterances than the literal agents, so they are more effective communicators. However, we start to run into out-of-memory errors with two layers of pragmatic reasoning. The test cases can be seen in `simple-pragmatics-amb.scm`. 

Future work on these agents will probably involve figuring out how to improve the efficiency of inference. 

References
----------
J. Beal. (2001). An Algorithm for Bootstrapping Communications. AI Memo 2001-016.

J. Beal. (2002). Generating Communications Systems Through Shared Context. Master's Thesis, AI Tech Report 2002-002.

M. C. Frank & N. D. Goodman. (2012). Predicting pragmatic reasoning in language games. Science 336: 998.

N. J. Smith, N. D. Goodman & M. C. Frank. (2013). Learning and using language via recursive pragmatic reasoning about other agents. Advances in Neural Information Processing Systems, 3039-3047.


