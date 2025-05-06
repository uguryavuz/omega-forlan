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

  fun extractUntilTok lts tok =
    let
      fun loop (acc, []) =
            raise Fail "extractUntilTok: targeted token not found"
        | loop (acc, (pos, tok') :: rest) =
            if CustomLex.equalTok (tok, tok') then (List.rev acc, rest)
            else loop ((pos, tok') :: acc, rest)
    in
      loop ([], lts)
    end

  fun inpORegExp0 lts =
    let
      val (oreg1, lts) = inpORegExp1 lts
    in
      case lts of
        (_, CustomLex.Plus) :: lts =>
          let val (oreg2, lts) = inpORegExp0 lts
          in (Union (oreg1, oreg2), lts)
          end
      | _ => (oreg1, lts)
    end

  and inpORegExp1 lts =
    case lts of
      (_, CustomLex.OpenPar) :: lts =>
        let
          val (oreg, lts) = inpORegExp0 lts
        in
          case lts of
            nil => CustomLex.errorNotEOFTerminated ()
          | (_, CustomLex.ClosPar) :: lts => (oreg, lts)
          | lt :: _ => (CustomLex.unexpectedTok lt)
        end
    | (pos, CustomLex.OpenBrack) :: lts =>
        let
          val (regLabToks, restLabToks) =
            extractUntilTok lts (CustomLex.ClosBrack)
          val regLabForlanToks =
            List.map
              (fn (pos, customTok) =>
                 (pos, CustomLex.customTokToForlanTok customTok)) regLabToks
          val (reg, leftover) = Reg.inputFromLabToks regLabForlanToks
          val _ =
            if List.null leftover then () else (Lex.unexpectedTok (hd leftover))
        in
          case restLabToks of
            (_, CustomLex.At) :: lts => (OmegaIter reg, lts)
          | _ =>
              let val (oreg, lts) = inpORegExp0 restLabToks
              in (Concat (reg, oreg), lts)
              end
        end
    | lt :: _ => (CustomLex.unexpectedTok lt)
    | nil => CustomLex.errorNotEOFTerminated ()

  val inputFromLabToks = inpORegExp0

  fun fromString s =
    case inpORegExp0 (CustomLex.lexString s) of
      (oreg, [(_, CustomLex.EOF)]) => (fromConcr oreg)
    | (_, nil) => Messages.cannotHappen ()
    | (_, lt :: _) => CustomLex.unexpectedTok lt

  fun input fil =
    case inpORegExp0 (CustomLex.lexFile fil) of
      (oreg, [(_, CustomLex.EOF)]) => (fromConcr oreg)
    | (_, nil) => Messages.cannotHappen ()
    | (_, lt :: _) => CustomLex.unexpectedTok lt

  (*********************************** Output **********************************)

  fun toString or =
    case or of
      OmegaIter r => "[" ^ Reg.toString r ^ "]@"
    | Union (or1, or2) => toString or1 ^ "+" ^ toString or2
    | Concat (r, or2) =>
        case or2 of
          Union (or2_1, or2_2) =>
            "[" ^ Reg.toString r ^ "](" ^ toString or2_1 ^ "+" ^ toString or2_2
            ^ ")"
        | _ => "[" ^ Reg.toString r ^ "]" ^ toString or2

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
