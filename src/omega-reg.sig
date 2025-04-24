signature OMEGA_REG =
sig
  datatype concr =
    OmegaIter of Reg.reg
  | Concat of Reg.reg * concr
  | Union of concr * concr
  type omegaReg
  val fromConcr: concr -> omegaReg
  val toConcr: omegaReg -> concr
  val toString: omegaReg -> string
  val output: omegaReg -> unit
  val omegaIter: Reg.reg -> omegaReg
  val concat: Reg.reg * omegaReg -> omegaReg
  val union: omegaReg * omegaReg -> omegaReg
  val isOmegaIter: omegaReg -> bool
  val isConcat: omegaReg -> bool
  val isUnion: omegaReg -> bool
  val genUnion: omegaReg list -> omegaReg
  val finUnionFormPairs: omegaReg -> (Reg.reg * Reg.reg) list
  val finUnionForm: omegaReg -> omegaReg
  val mapSubReg: (Reg.reg -> Reg.reg) -> omegaReg -> omegaReg
end
