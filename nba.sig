signature NBA =
sig
  type concr =
    { states: Sym.sym Set.set
    , starts: Sym.sym Set.set
    , accepts: Sym.sym Set.set
    , trans: Tran.tran Set.set
    }
  type nba
  val valid: concr -> bool
  val fromConcr: concr -> nba
  val toConcr: nba -> concr
  val injToNFA: nba * Sym.sym -> NFA.nfa
  val projFromNFA: NFA.nfa -> nba
  val fromString: string -> nba
  val input: string -> nba
  val toPP: nba -> PP.pp
  val toString: nba -> string
  val output: string * nba -> unit
end
