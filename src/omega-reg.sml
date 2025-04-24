structure OmegaReg :> OMEGA_REG =
struct

  (******************************** Main Types *********************************)
  
  datatype concr =
    OmegaIter of Reg.reg
  | Concat of Reg.reg * concr
  | Union of concr * concr

  type omegaReg = concr

  fun check (OmegaIter r) =
        if Reg.hasEmp r then
          raise Fail "OmegaIter argument must not accept the empty string"
        else
          ()
    | check (Concat (_, or)) = check or
    | check (Union (or1, or2)) =
        (check or1; check or2)

  fun fromConcr (c: concr) : omegaReg =
    (check c; c)

  fun toConcr (or: omegaReg) : concr = or

  (*********************************** Input ***********************************)


  (*********************************** Output **********************************)
  
  fun toString (OmegaIter r) =
        "[" ^ Reg.toString r ^ "]@"
    | toString (Concat (r, or)) =
        "[" ^ Reg.toString r ^ "]" ^ toString or
    | toString (Union (or1, or2)) =
        toString or1 ^ "+" ^ toString or2

  fun output or =
    (print (toString or); print PP.newline)

  (****************************** Other Functions ******************************)

  fun omegaIter (r: Reg.reg) : omegaReg =
    if Reg.hasEmp r then
      raise Fail "OmegaIter argument must not accept the empty string"
    else
      OmegaIter r

  fun concat (r: Reg.reg, or: omegaReg) : omegaReg = Concat (r, or)

  fun union (or1: omegaReg, or2: omegaReg) : omegaReg = Union (or1, or2)

  fun isOmegaIter (OmegaIter _) = true
    | isOmegaIter _ = false

  fun isConcat (Concat _) = true
    | isConcat _ = false

  fun isUnion (Union _) = true
    | isUnion _ = false

  fun genUnion nil =
        raise Fail "genUnion for omega-regexps should not be called with nil"
    | genUnion [or] = or
    | genUnion (or :: ors) =
        Union (or, genUnion ors)

  fun finUnionFormPairs (OmegaIter r) = [(Reg.emptyStr, r)]
    | finUnionFormPairs (Union (or1, or2)) =
        finUnionFormPairs or1 @ finUnionFormPairs or2
    | finUnionFormPairs (Concat (r, or)) =
        map (fn (r1, r2) => (Reg.concat (r, r1), r2)) (finUnionFormPairs or)

  fun finUnionForm or =
    genUnion
      (map (fn (r1, r2) => Concat (r1, OmegaIter r2)) (finUnionFormPairs or))

  fun mapSubReg (f: Reg.reg -> Reg.reg) : omegaReg -> omegaReg =
    fn OmegaIter r => OmegaIter (f r)
     | Concat (r, or') => Concat (f r, mapSubReg f or')
     | Union (or1, or2) => Union (mapSubReg f or1, mapSubReg f or2)

end
