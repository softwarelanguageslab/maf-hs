# Communication contracts simplified: a transformation to λα with dynamic parameters

This repository aims to simplify the semantics of communication contracts (Vandenbogaerde et al., 2024) by transforming programs in λα/c to programs in λα. 
This simplifies the semantics by expressing it into well-understood constructs, as well as derrived tooling (e.g., static analysis).


## Syntax of λα

λα is an extension of the untyped lambda calculus. This extension includes constructs to express actor systems 
and their communication. To this end, the language is extended with a construct to spawn new actors
given a message handler in the form of a closure. Moreover, two additional special constructs are added 
to send and receive messages respectively. 

In contrast to the classic actor model, our actor model does not feature lists of message
handlers indexed by message tag. Instead, our actor system is modelled after *Erlang* which 
treats messages as ordinary values that are shared between processes using a special 
message sending operator '!'. However, our message sending operator is not part of 
the formal syntax of the language, instead it is provided as a **primitive function** 
so that it can be overshadowed for contract checking purposes.

λα also contains constructs for *dynamic parameters* (cf. Racket `parameters`), which allow
variables to change value based on the dynamic extent rather than on lexical scoping.

Thus we arrive at the following formal syntax of our language:

$$
\begin{aligned}
e \in Exp &::= & \lambda x . e 
          &\mid& e~e 
          &\mid& \mathsf{spawn}~e
          &\mid& \mathsf{terminate}
          &\mid& \mathsf{self}
          &\mid& (e, e)
          &\mid& \mathsf{parameter}~e
\end{aligned}
$$

## Translation from CPCF



## Structure of the artifact

