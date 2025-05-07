# Documentation for `omega-forlan`

This document describes the main modules implemented in this repository: [`OmegaReg`](#module-omegareg) for ω-regular expressions and [`NBA`](#module-nba) for non-deterministic Büchi automata. See the end of this document for a list of [references](#references).

---

## Module: `OmegaReg`

### Overview

The `OmegaReg` module implements the infinite fragment of ω-regular expressions. It supports parsing, pretty-printing, structural manipulation, and conversions.

### Types

- `datatype concr`  
  The concrete datatype of the infinite fragment of ω-regular expressions, defined as:
  - `OmegaIter of Reg.reg`  
    For a regular expression $A$, $A^\omega$ is called the ω-iteration of $A$, generating the set of infinite words $w_1 w_2 \ldots$ such that $w_i \in \mathcal{L}(A)$ for all $i$. This operation is only defined for expressions $A$ that do not produce the empty string.

  - `Concat of Reg.reg * concr`  
    Concatenation is not defined for two infinite words, but it is defined for a finite word followed by an infinite word. Hence, for a regular expression $A$ and an ω-regular expression $B$, $A B$ is the concatenation of $A$ and $B$, generating the set of infinite words $w_A w_B$ such that $w_A$ is a finite word in $\mathcal{L}(A)$ and $w_B$ is an infinite word in $\mathcal{L}(B)$.

  - `Union of concr * concr`   
    The union of two ω-regular expressions is defined similarly to the union of two regular expressions. For ω-regular expressions $A$ and $B$, $A \cup B$ is the union of $A$ and $B$, generating the set of infinite words in $\mathcal{L}(A) \cup \mathcal{L}(B)$.

- `type omegaReg`  
  The abstract type of ω-regular expressions, which is a "well-formed" `concr`.

### Construction

- `fromConcr : concr -> omegaReg`     
  Converts a `concr` to `omegaReg` checking well-formedness. Namely, it checks that all `OmegaIter` subexpressions are well-formed in that their arguments do not produce the empty string.

- `toConcr : omegaReg -> concr`  
  Cast `omegaReg` to `concr`.

### Parsing and I/O
For the string representation of ω-regular expressions, we adhere to the following syntax:
  - `[r]@` represents `OmegaIter r`, where `r` is a regular expression.
  - `[r]r'` represents `Concat (r,r')`, where `r` is a regular expression and `r'` is an ω-regular expression.
  - `r+r'` represents `Union (r,r')`, where `r` and `r'` are both ω-regular expressions.
  - `(...)` is used for grouping.

The following functions are provided for parsing and I/O:

- `inputFromLabToks : (int * CustomLex.tok) list -> omegaReg * (int * CustomLex.tok) list`  
  Tries to input an ω-regular expression from a list of tokens, consuming as much of its input as possible, and returning the pair of that expression and the rest of the input. Issues an error message if this fails. The implementation leverages the `CustomLex` module, and the regular expression parser from Forlan to handle the regular subexpressions.

- `fromString : string -> omegaReg`  
  Parses an ω-regular expression from a string.

  For instance, to input the ω-regular expression $(0+1)^* (0^{\omega} + 1^{\omega})$, which generates the set of infinite words that begin with a finite bitstring followed either by infinite 0s or infinite 1s, would be input via:
  ```sml
  OmegaReg.fromString "[(0+1)*]([0]@+[1]@)"
  ```

- `input : string -> omegaReg`  
  Parses an ω-regular expression from a file.

- `toString : omegaReg -> string`  
  Pretty-prints an ω-regular expression to a string.

- `output : string * omegaReg -> unit`  
  Pretty-prints an ω-regular expression to a file.

### Smart constructors and structural predicates

- `emptySet : omegaReg`   
  The ω-regular expression that generates the empty language. Internally represented as `OmegaIter (Reg.emptySet)`.

- `omegaIter : Reg.reg -> omegaReg`   
  Returns `OmegaIter r` for a regular expression `r`. Checks that `r` does not produce the empty string.  

- `concat : Reg.reg * omegaReg -> omegaReg`   
  Returns `Concat (r,r')` for a regular expression `r` and an ω-regular expression `r'`.

- `union : omegaReg * omegaReg -> omegaReg`   
  Returns `Union (r,r')` for two ω-regular expressions `r` and `r'`.

- `isOmegaIter : omegaReg -> bool`    
  Checks if the ω-regular expression is a well-formed `OmegaIter`. Note that it raises an error if is a non-well-formed `OmegaIter`.

- `isConcat : omegaReg -> bool`   
  Checks if the ω-regular expression is a `Concat`.

- `isUnion : omegaReg -> bool`  
  Checks if the ω-regular expression is a `Union`.

### Canonical forms and utilities

- `genUnion : omegaReg list -> omegaReg`  
  Takes a list of ω-regular expressions and returns their left-associated union. Returns `emptySet` if the list is empty.

- `toFinUnionPairs : omegaReg -> (Reg.reg * Reg.reg) list`  
  We note that every ω-regular expression (in the infinite fragment) can be expressed as a finite union $\bigcup_{i} A_i B_i^{\omega}$, where $A_i$ and $B_i$ are regular expressions. This is a well-known result, e.g. refer to Theorem 3.2. in [[Perrin and Pin, 2004]](#references). Then, `toFinUnionPairs` returns the corresponding list of pairs of regular expressions  $(A_i, B_i)$ for a given ω-regular expression. 

- `fromFinUnionPairs : (Reg.reg * Reg.reg) list -> omegaReg`  
  Takes a list of pairs of regular expressions $(A_i, B_i)$ and returns the corresponding ω-regular expression $\bigcup_{i} A_i B_i^{\omega}$.

- `toFinUnionForm : omegaReg -> omegaReg`   
  Converts an arbitrary ω-regular expression into its canonical finite union form. 

  For instance, for $(0+1)^* (0^{\omega} + 1^{\omega})$, this should return the ω-regular expression $(0+1)^* 0^{\omega} + (0+1)^* 1^{\omega}$. We can confirm that this is indeed the case by running:
  ```sml
  - OmegaReg.fromString "[(0+1)*]([0]@+[1]@)";
  val it = - : OmegaReg.omegaReg
  - OmegaReg.toFinUnionForm it;
  val it = - : OmegaReg.omegaReg
  - OmegaReg.output("", it);
  [(0 + 1)*][0]@ + [(0 + 1)*][1]@
  ```

- `mapSubReg : (Reg.reg -> Reg.reg) -> omegaReg -> omegaReg`  
  Applies a transformation to all regular subexpressions inside the ω-regular expression, preserving the structure of the ω-regular expression. This is currently not used in the library, but it might be useful for future extensions regarding the simplification of ω-regular expressions that incorporates Forlan's simplification algorithms of regular expressions. Such a simplification procedure would also use equivalences between ω-regular expressions such as those noted in Section 3 of [[Perrin and Pin, 2004]](#references).

---

## Module: `NBA`

### Overview

The `NBA` module defines non-deterministic Büchi automata (NBAs), supporting standard automata operations, conversion from/to ω-regular expressions, and decision procedures. The implementation is based on Forlan's `NFA` module (for simplicity, we stuck to single-symbol transitions). 

### Types

- `type concr`  
  The concrete type of (not necessarily well-formed) non-deterministic Büchi automata, defined as record types:
  ```sml
  { states: Sym.sym Set.set,
    starts: Sym.sym Set.set,
    accepts: Sym.sym Set.set,
    trans: Tran.tran Set.set }
  ```
  Note that this allows for multiple start states, as opposed to the Forlan convention of automata with single start states. This is to match the conventional presentation of Büchi automata, which is almost always done with multiple start states.

  The acceptance condition for a non-deterministic Büchi automaton is that there exists an infinite run that visits an accepting state infinitely often. This is not the same as the acceptance condition for finite automata, which requires that the run ends in an accepting state. As we cannot process arbitrary infinite words, we have not implemented an acceptance checking procedure. 

- `type nba`  
  Abstract type of well-formed Büchi automata, with checks that the start states and the accepting states are non-empty, and that the transitions are between states in the automaton. 

### Construction, parsing and I/O

- `valid : concr -> bool`  
  Checks well-formedness of the automaton as explained above.

- `fromConcr : concr -> nba`  
  Creates a validated `nba`.

- `toConcr : nba -> concr`  
  Cast `nba` to `concr`.

- `injToNFA : nba * Sym.sym -> NFA.nfa`  
  Converts an NBA to an NFA with the specified start state.

- `projFromNFA : NFA.nfa -> nba`  
  Converts an NFA to an NBA. The start state is turned into a singleton. 

- `fromString : string -> nba`  
  Parses a string to an NBA. This is almost verbatim the same as Forlan's `NFA.fromString`, but adjusted to multiple start states. Hence, the syntax is the same as in Forlan, except you now declare `{start states}` followed by a set of start states.

- `input : string -> nba`  
  Parses an NBA from a file. 

- `toPP : nba -> PP.pp`  
  Returns a pretty-printing expression for the automaton.

- `toString : nba -> string`  
  Pretty-prints the automaton to a string.

- `output : string * nba -> unit`  
  Pretty-prints the automaton to a file.

### Structural access and manipulation

- `states : nba -> Sym.sym Set.set`   
  Returns the set of states of the automaton.
- `startStates : nba -> Sym.sym Set.set`    
  Returns the set of start states of the automaton.
- `acceptingStates : nba -> Sym.sym Set.set`  
  Returns the set of accepting states of the automaton.
- `transitions : nba -> Tran.tran Set.set`  
  Returns the set of transitions of the automaton.
- `alphabet : nba -> Sym.sym Set.set`   
  Returns the alphabet of the automaton.
- `renameStates : nba * SymRel.sym_rel -> nba`  
  Renames the states of the automaton using the given bijection. Adapted from Forlan's `NFA.renameStates`.
- `renameStatesCanonically : nba -> nba`  
  Canonically renames the states of the automaton. Adapted from Forlan's `NFA.renameStatesCanonically`.

### NBA/ω-regular expression conversions

An ω-regular language can be recognized by a non-deterministic Büchi automaton (NBA), and vice versa. 
This was established in Büchi's seminal work [[Büchi, 1966]](#references) and is occasionally referred to as _Büchi's characterization theorem_.

- `fromOmegaReg : OmegaReg.omegaReg -> nba`  
  Converts an ω-regular expression to an NBA, using the implementation of `union`, `concat`, and `omegaIter` described in the next section.
- `toOmegaReg : nba -> OmegaReg.omegaReg`
  Converts an NBA to an ω-regular expression using the construction from Büchi's seminal work.
  The implementation follows the description of this construction in [[Finkbeiner and Schewe, 2008]](#references),
  specifically in the notes for Lecture 2.

### Operations

The `union`, `concat`, and `omegaIter` operations are implemented as described in [[Finkbeiner and Schewe, 2008]](#references).

- `union : nba * nba -> nba`  
  Returns the union of two NBAs. 
- `concat : NFA.nfa * nba -> nba`  
  Returns the concatenation of an NFA and an NBA. 
- `omegaIter : NFA.nfa -> nba`  
  Returns the ω-iteration of an NFA. 
- `inter : nba * nba -> nba`  
  Returns the intersection of two NBAs. This construction is based on the work of [[Choueka, 1974]](#references). The implementation follows the description of this construction in [[Finkbeiner and Schewe, 2008]](#references): however, this particular description contains a typo in the fourth case of its transitions which has been corrected in the implementation. 
- `complement : nba * Sym.sym Set.set * bool -> nba`  
  Returns the complement of an NBA for a given alphabet. Namely, given an NBA $A$ and an alphabet $\Sigma$, the complement of $A$ accepts $\Sigma^{\omega} \setminus \mathcal{L}(A)$. The third argument is a Boolean flag controlling the verbosity of the command's output. 

  We note that complementation for Büchi automata is a non-trivial operation, since they are not deterministic and Büchi automata are not necessarily determinizable. (In particular, deterministic Büchi automata recognize a proper subset of ω-regular languages. See Section 6 of [[Perrin and Pin, 2004]](#references) for examples.)  It is known to be an expensive operation, with a complexity lower-bound that is exponential in the number of states. We follow the construction of [[Friedgut et al., 2004]](#references), which is a faster version of the construction in [[Kupferman and Vardi, 2001]](#references). An earlier version of this project used the construction by Kupferman and Vardi, but it was found to be too slow. In the implementation, we in particular follow the description of this construction in Proposition 2.2. of [[Schewe, 2009]](#references). In fact, in this work, Schewe describes an even faster construction that builds on the construction of [[Friedgut et al., 2004]](#references). This can easily be adapted to our implementation.

  The theoretical lower bound of $O(0.76n)^n$ for this operation was later achieved by [[Allred and Ultes-Nitsche, 2018]](#references), who describe a construction that is optimal. This construction could also be implemented in a later version of this project.

### Decision procedures

The following procedures are fairly simple to implement (all less than a few lines of code) given the availability of the operations and conversions described above. They all follow observations made in Chapter 3 of [[Khoussainov and Nerode, 2001]](#references).

- `isEmpty : nba -> bool`
  Checks if the language of the automaton is empty. The implementation follows Theorem 3.9.1 of [[Khoussainov and Nerode, 2001]](#references).
- `equivalent : nba * nba -> bool`  
  Checks if two automata are equivalent. The implementation follows Theorem 3.9.2 of [[Khoussainov and Nerode, 2001]](#references).
- `isUniversal : nba * Sym.sym Set.set -> bool`
  Checks if the language of the automaton is universal for a given alphabet (i.e., given $\Sigma$, whether the language is $\Sigma^{\omega}$). The implementation follows Theorem 3.9.3 of [[Khoussainov and Nerode, 2001]](#references).

## References

- [**[Allred and Ultes-Nitsche, 2018]**](https://doi.org/10.1145/3209108.3209138)   
  J. D. Allred and U. Ultes-Nitsche, "A Simple and Optimal Complementation Algorithm for Büchi Automata," in Proceedings of the 33rd Annual ACM/IEEE Symposium on Logic in Computer Science, Oxford United Kingdom: ACM, Jul. 2018, pp. 46–55. 

- [**[Büchi, 1966]**](https://doi.org/10.1016/S0049-237X(09)70564-6)  
  J. R. Büchi, "Symposium on Decision Problems: On a Decision Method in Restricted Second Order Arithmetic," in Studies in Logic and the Foundations of Mathematics, vol. 44, Elsevier, 1966, pp. 1–11.

- [**[Choueka, 1974]**](https://doi.org/10.1016/S0022-0000(74)80051-6)  
  Y. Choueka, "Theories of automata on ω-tapes: A simplified approach," Journal of Computer and System Sciences, vol. 8, no. 2, pp. 117–141, Apr. 1974.

- [**[Finkbeiner and Schewe, 2008]**](https://finkbeiner.groups.cispa.de/teaching/automata-games-verification-08/)  
  B. Finkbeiner and S. Schewe, "Automata, Games and Verification: Lecture Notes," April 2008.

- [**[Friedgut et al., 2004]**](https://doi.org/10.1007/978-3-540-30476-0_10)   
  E. Friedgut, O. Kupferman, and M. Y. Vardi, "Büchi Complementation Made Tighter," in Automated Technology for Verification and Analysis, vol. 3299, F. Wang, Ed., in Lecture Notes in Computer Science, vol. 3299. , Berlin, Heidelberg: Springer Berlin Heidelberg, 2004, pp. 64–78.

- [**[Khoussainov and Nerode, 2001]**](https://doi.org/10.1007/978-1-4612-0171-7)   
  B. Khoussainov and A. Nerode, "Automata Theory and its Applications," in Progress in Computer Science and Applied Logic, no. 21. Boston, MA: Birkhäuser Boston, 2001.

- [**[Kupferman and Vardi, 2001]**](https://doi.org/10.1145/377978.377993)  
  O. Kupferman and M. Y. Vardi, "Weak alternating automata are not that weak," ACM. Trans. Comput. Logic, vol. 2, no. 3, pp. 408–429, Jul. 2001.

- [**[Perrin and Pin, 2004]**](https://www.irif.fr/~jep/PDF/InfiniteWords/Chapter1.pdf)   
  D. Perrin and J.-E. Pin, "Automata and Infinite Words," in Pure and Applied Mathematics, vol. 141, Elsevier, 2004, pp. 5–73.

- [**[Schewe, 2009]**](https://doi.org/10.4230/LIPICS.STACS.2009.1854)  
  S. Schewe, "Büchi Complementation Made Tight," LIPIcs, Volume 3, STACS 2009, vol. 3, pp. 661–672, 2009.
