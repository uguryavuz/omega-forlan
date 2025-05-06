signature OMEGA_REG =
sig
  datatype concr =
    OmegaIter of Reg.reg
  | Concat of Reg.reg * concr
  | Union of concr * concr
  type omegaReg
  val fromConcr: concr -> omegaReg
  val toConcr: omegaReg -> concr
  val inputFromLabToks: (int * CustomLex.tok) list
                        -> omegaReg * (int * CustomLex.tok) list
  val fromString: string -> omegaReg
  val input: string -> omegaReg
  val toString: omegaReg -> string
  val output: string * omegaReg -> unit
  val emptySet: omegaReg
  val omegaIter: Reg.reg -> omegaReg
  val concat: Reg.reg * omegaReg -> omegaReg
  val union: omegaReg * omegaReg -> omegaReg
  val isOmegaIter: omegaReg -> bool
  val isConcat: omegaReg -> bool
  val isUnion: omegaReg -> bool
  val genUnion: omegaReg list -> omegaReg
  val toFinUnionPairs: omegaReg -> (Reg.reg * Reg.reg) list
  val fromFinUnionPairs: (Reg.reg * Reg.reg) list -> omegaReg
  val toFinUnionForm: omegaReg -> omegaReg
  val mapSubReg: (Reg.reg -> Reg.reg) -> omegaReg -> omegaReg
end
