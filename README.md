A Library for/of Communicating Machines
=============

Project for 6.945, Spring 2014
James Duyck and Richard Futrell

Experimental Setup
------------------


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


