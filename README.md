# omega-forlan  
Uğur Yavuz, May 2025

This repository builds on the [Forlan](https://alleystoughton.us/forlan/) library, extending it to support ω-regular expressions and non-deterministic Büchi automata. Forlan is assumed to be installed.

## Repository structure

- `omega-forlan`  
  Bash script that launches Forlan and loads the contents of this repository.

- `src/mod/lex(.sig|.sml)`  
  A customized version of Forlan's `Lex` module, extended to support additional characters—namely `[`, `]`, and `@`—required for parsing string representations of ω-regular expressions.

- `src/omega-reg(.sig|.sml)`  
  Implements the infinite fragment of ω-regular expressions. The design follows Forlan's `Reg` module.

- `src/nba(.sig|.sml)`  
  Implements non-deterministic Büchi automata (NBA), based on Forlan's `NFA` module. In addition to standard operations and conversions between NBA and ω-regular expressions, which follow from Büchi's seminal work [[Büchi, 1962]](https://link.springer.com/chapter/10.1007/978-1-4613-8928-6_23), this module also supports:
  - Intersection [[Choueka, 1974]](https://www.sciencedirect.com/science/article/pii/S0022000074800516)
  - Complementation [[Friedgut–Kupferman–Vardi, 2004]](http://link.springer.com/10.1007/978-3-540-30476-0_10)
  - Additional derived operations

## Documentation

See [`DOC.md`](DOC.md) for implementation notes and usage details.
