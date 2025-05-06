signature CUSTOM_LEX =
sig
  datatype tok =
    Bar
  | Comma
  | Dollar
  | Perc
  | Plus
  | Semicolon
  | Star
  | Tilde
  | OpenPar
  | ClosPar
  | OpenBrack
  | ClosBrack
  | At
  | SingArr
  | DoubArr
  | Sym of Lex.sym
  | Heading of string
  | EOF
  val customTokToForlanTok: tok -> Lex.tok
  val equalTok: tok * tok -> bool
  val errorNotEOFTerminated: unit -> 'a
  val expectedTok: int * tok -> 'a
  val expectedDigit: int -> 'a
  val expectedLetter: int -> 'a
  val expectedLetterOrDigit: int -> 'a
  val unexpectedTok: int * tok -> 'a
  val checkInLabToks: tok * (int * tok) list -> (int * tok) list
  val error: int * int * PP.pp list -> 'a
  val lexString: string -> (int * tok) list
  val lexFile: string -> (int * tok) list
end
