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
  val states: nba -> Sym.sym Set.set
  val startStates: nba -> Sym.sym Set.set
  val acceptingStates: nba -> Sym.sym Set.set
  val transitions: nba -> Tran.tran Set.set
  val renameStates: nba * SymRel.sym_rel -> nba
  val renameStatesCanonically: nba -> nba
  val union: nba * nba -> nba
  val concat: NFA.nfa * nba -> nba
  val omegaIter: NFA.nfa -> nba
  val fromOmegaReg: OmegaReg.omegaReg -> nba
  val toOmegaReg: nba -> OmegaReg.omegaReg
end
