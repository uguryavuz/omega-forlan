# omega-forlan  
Uğur Yavuz, May 2025

This repository builds on the [Forlan](https://alleystoughton.us/forlan/) library, extending it to support ω-regular expressions and non-deterministic Büchi automata. Forlan is assumed to be installed.

## Repository structure

- `omega-forlan`  
  Bash script that launches Forlan and loads the contents of this repository. To run, make sure that the script is executable by running `chmod +x ./omega-forlan` and then execute it with `./omega-forlan`.

- `src/mod/lex(.sig|.sml)`  
  A customized version of Forlan's `Lex` module, extended to support additional characters—namely `[`, `]`, and `@`—required for parsing string representations of ω-regular expressions.

- `src/omega-reg(.sig|.sml)`  
  Implements the infinite fragment of ω-regular expressions. The design follows Forlan's `Reg` module.

- `src/nba(.sig|.sml)`  
  Implements non-deterministic Büchi automata (NBA), based on Forlan's `NFA` module. In addition to standard operations and conversions between NBA and ω-regular expressions, which follow from Büchi's seminal work [[Büchi, 1966]](DOC.md#references), this module also supports:
  - Intersection [[Choueka, 1974]](DOC.md#references)
  - Complementation [[Friedgut et al., 2004]](DOC.md#references)
  - Additional derived operations

## Documentation

See [`DOC.md`](DOC.md) for implementation notes and usage details.
