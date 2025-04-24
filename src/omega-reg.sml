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

  val emptySet = OmegaIter (Reg.emptySet)

  fun omegaIter (r: Reg.reg) : omegaReg =
    if Reg.hasEmp r then
      raise Fail "OmegaIter argument must not accept the empty string"
    else
      OmegaIter r

  fun concat (r: Reg.reg, or: omegaReg) : omegaReg = Concat (r, or)

  fun union (or1: omegaReg, or2: omegaReg) : omegaReg = Union (or1, or2)

  fun isOmegaIter (OmegaIter r) =
        (check (OmegaIter r); true)
    | isOmegaIter _ = false

  fun isConcat (Concat _) = true
    | isConcat _ = false

  fun isUnion (Union _) = true
    | isUnion _ = false

  fun genUnion nil = emptySet
    | genUnion [or] = or
    | genUnion (or :: ors) =
        if (List.null ors) then or else union (or, genUnion ors)

  fun toFinUnionPairs (OmegaIter r) =
        (check (OmegaIter r); [(Reg.emptyStr, r)])
    | toFinUnionPairs (Union (or1, or2)) =
        toFinUnionPairs or1 @ toFinUnionPairs or2
    | toFinUnionPairs (Concat (r, or)) =
        map
          (fn (r1, r2) =>
             (if (Reg.isEmptyStr r1) then (r, r2) else (Reg.concat (r, r1), r2)))
          (toFinUnionPairs or)

  fun fromFinUnionPairs pairs =
    genUnion (map (fn (r1, r2) => concat (r1, omegaIter r2)) pairs)

  fun toFinUnionForm or =
    fromFinUnionPairs (toFinUnionPairs or)

  fun mapSubReg (f: Reg.reg -> Reg.reg) : omegaReg -> omegaReg =
    fn OmegaIter r => (check (OmegaIter r); OmegaIter (f r))
     | Concat (r, or') => Concat (f r, mapSubReg f or')
     | Union (or1, or2) => Union (mapSubReg f or1, mapSubReg f or2)

end
