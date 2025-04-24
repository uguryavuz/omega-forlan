structure NBA :> NBA =
struct
  structure M = Messages
  structure L = Lex

  (******************************** Main Types *********************************)

  type concr =
    { states: Sym.sym Set.set
    , starts: Sym.sym Set.set
    , accepts: Sym.sym Set.set
    , trans: Tran.tran Set.set
    }

  type nba = concr

  fun checkStarts (nil, _) = ()
    | checkStarts (q :: qs, states) =
        if not (SymSet.memb (q, states)) then
          M.errorPP (fn () =>
            [ PP.fromString "invalid"
            , PP.fromString "start"
            , PP.fromString "state:"
            , PP.quote (Sym.toPP q)
            ])
        else
          checkStarts (qs, states)

  fun checkAccepts (nil, _) = ()
    | checkAccepts (q :: qs, states) =
        if not (SymSet.memb (q, states)) then
          M.errorPP (fn () =>
            [ PP.fromString "invalid"
            , PP.fromString "accepting"
            , PP.fromString "state:"
            , PP.quote (Sym.toPP q)
            ])
        else
          checkAccepts (qs, states)

  fun checkTrans (nil, _) = ()
    | checkTrans ((q, x, r) :: trans, states) =
        if not (SymSet.memb (q, states)) then
          M.errorPP (fn () =>
            [ PP.fromString "invalid"
            , PP.fromString "state"
            , PP.fromString "in"
            , PP.fromString "transition:"
            , PP.quote (Sym.toPP q)
            ])
        else if not (SymSet.memb (r, states)) then
          M.errorPP (fn () =>
            [ PP.fromString "invalid"
            , PP.fromString "state"
            , PP.fromString "in"
            , PP.fromString "transition:"
            , PP.quote (Sym.toPP r)
            ])
        else if length x <> 1 then
          M.errorPP (fn () =>
            [ PP.fromString "invalid"
            , PP.fromString "label"
            , PP.fromString "in"
            , PP.fromString "transition:"
            , PP.quote (Str.toPP x)
            ])
        else
          checkTrans (trans, states)

  fun check (c: concr) =
    let
      val states = #states c
      val starts = #starts c
      val accepts = #accepts c
      val trans = #trans c
    in
      ( checkStarts (Set.toList starts, states)
      ; checkAccepts (Set.toList accepts, states)
      ; checkTrans (Set.toList trans, states)
      )
    end

  fun valid concr =
    (M.quiet (fn () => check concr); true)
    handle _ => false

  fun fromConcr (concr: concr) : nba =
    (check concr; concr)

  fun toConcr (nba: nba) : concr = nba

  fun injToNFA (nba: nba, start: Sym.sym) : NFA.nfa =
    if not (SymSet.memb (start, #states nba)) then
      M.errorPP (fn () =>
        [ PP.fromString "invalid"
        , PP.fromString "start"
        , PP.fromString "state:"
        , PP.quote (Sym.toPP start)
        ])
    else
      NFA.projFromFA (FA.fromConcr
        { stats = #states nba
        , start = start
        , accepting = #accepts nba
        , trans = #trans nba
        })

  fun projFromNFA (nfa: NFA.nfa) : nba =
    let
      val concr = FA.toConcr (NFA.injToFA nfa)
      val states = #stats concr
      val starts = Set.sing (#start concr)
      val accepts = #accepting concr
      val trans = #trans concr
    in
      fromConcr
        {states = states, starts = starts, accepts = accepts, trans = trans}
    end

  (*********************************** Input ***********************************)

  fun inpNBA lts =
    let
      val lts = L.checkInLabToks (L.Heading "{states}", lts)
      val (states, lts) = SymSet.inputFromLabToks lts
      val lts = L.checkInLabToks (L.Heading "{startstates}", lts)
      val (starts, lts) = SymSet.inputFromLabToks lts
      val lts = L.checkInLabToks (L.Heading "{acceptingstates}", lts)
      val (accepts, lts) = SymSet.inputFromLabToks lts
      val lts = L.checkInLabToks (L.Heading "{transitions}", lts)
      val (trans, lts) = TranSet.inputFromLabToks lts
      val concr =
        {states = states, starts = starts, accepts = accepts, trans = trans}
    in
      (fromConcr concr, lts)
    end

  fun fromString s =
    case inpNBA (L.lexString s) of
      (fa, [(_, L.EOF)]) => fa
    | (_, nil) => M.cannotHappen ()
    | (_, lt :: _) => L.unexpectedTok lt

  fun input fil =
    case inpNBA (L.lexFile fil) of
      (fa, [(_, L.EOF)]) => fa
    | (_, nil) => M.cannotHappen ()
    | (_, lt :: _) => L.unexpectedTok lt

  (*********************************** Output **********************************)

  fun toPP {states, starts, accepts, trans} =
    PP.block
      ( true
      , [ PP.block (true, [PP.fromString "{states}", SymSet.toPP states])
        , PP.block (true, [PP.fromString "{start states}", SymSet.toPP starts])
        , PP.block
            (true, [PP.fromString "{accepting states}", SymSet.toPP accepts])
        , PP.block (true, [PP.fromString "{transitions}", TranSet.toPP trans])
        ]
      )

  val toString = PP.toString o toPP

  fun output ("", fa) =
        (print (toString fa); print PP.newline)
    | output (fil, fa) =
        let
          val opt = (SOME (TextIO.openOut fil) handle _ => NONE)
        in
          case opt of
            NONE =>
              M.errorPP (fn () =>
                [ PP.fromString "unable"
                , PP.fromString "to"
                , PP.fromString "open"
                , PP.fromString "file:"
                , PP.quote (PP.fromStringSplitEscape fil)
                ])
          | SOME stm =>
              ( TextIO.output (stm, toString fa)
              ; TextIO.output (stm, PP.newline)
              ; TextIO.closeOut stm
              )
        end

  (****************************** Other Functions *****************************)

  fun states (nba: nba) = #states nba

  fun startStates (nba: nba) = #starts nba

  fun acceptingStates (nba: nba) = #accepts nba

  fun transitions (nba: nba) = #trans nba

  fun renameStates (nba: nba, rel: SymRel.sym_rel) =
    let
      val states = #states nba
      val _ =
        if not (SymRel.bijectionFromAvoiding (rel, states, Set.empty)) then
          M.errorString (fn () =>
            ["invalid", "states", "renaming", "for", "NBA"])
        else
          ()
      val starts = #starts nba
      val accepts = #accepts nba
      val trans = #trans nba
    in
      let
        val renam = SymRel.applyFunction rel
        val states' = SymSet.map renam states
        val starts' = SymSet.map renam starts
        val accepts' = SymSet.map renam accepts
        val trans' = TranSet.map (fn (q, x, r) => (renam q, x, renam r)) trans
      in
        fromConcr
          { states = states'
          , starts = starts'
          , accepts = accepts'
          , trans = trans'
          }
      end
    end

  fun renameStatesCanonically (nba: nba) =
    let
      val states = #states nba
      fun pos q =
        valOf (Set.position (fn p => Sym.equal (p, q)) states)
      val renam =
        if (Set.size states <= 26) then
          fn q => Sym.fromString (str (chr (ord #"A" + pos q - 1)))
        else
          fn q =>
            Sym.fromString (String.concat ["<", Int.toString (pos q), ">"])
      val rel = SymRel.mlFunctionToFunction (renam, states)
    in
      renameStates (nba, rel)
    end

  fun union (nba1: nba, nba2: nba) =
    let
      fun renam1 a =
        Sym.fromTop (Sym.Compound [SOME (Sym.fromString "1"), NONE, SOME a])
      fun renam2 a =
        Sym.fromTop (Sym.Compound [SOME (Sym.fromString "2"), NONE, SOME a])
      val rel1 = SymRel.mlFunctionToFunction (renam1, #states nba1)
      val rel2 = SymRel.mlFunctionToFunction (renam2, #states nba2)
      val nba1' = renameStates (nba1, rel1)
      val nba2' = renameStates (nba2, rel2)
    in
      fromConcr
        { states = SymSet.union (#states nba1', #states nba2')
        , starts = SymSet.union (#starts nba1', #starts nba2')
        , accepts = SymSet.union (#accepts nba1', #accepts nba2')
        , trans = TranSet.union (#trans nba1', #trans nba2')
        }
    end

  fun concat (nfa: NFA.nfa, nba: nba) =
    let
      fun renam1 a =
        Sym.fromTop (Sym.Compound [SOME (Sym.fromString "1"), NONE, SOME a])
      fun renam2 a =
        Sym.fromTop (Sym.Compound [SOME (Sym.fromString "2"), NONE, SOME a])
      val rel1 = SymRel.mlFunctionToFunction (renam1, NFA.states nfa)
      val rel2 = SymRel.mlFunctionToFunction (renam2, #states nba)
      val nfa' = NFA.renameStates (nfa, rel1)
      val nfa'_states = NFA.states nfa'
      val nfa'_start = NFA.startState nfa'
      val nfa'_accepts = NFA.acceptingStates nfa'
      val nfa'_trans = NFA.transitions nfa'
      val nba' = renameStates (nba, rel2)
      val nba'_states = #states nba'
      val nba'_starts = #starts nba'
      val nba'_accepts = #accepts nba'
      val nba'_trans = #trans nba'
    in
      let
        val starts =
          if not (SymSet.memb (nfa'_start, nfa'_accepts)) then
            Set.sing nfa'_start
          else
            SymSet.union (Set.sing nfa'_start, nba'_starts)
        val filtTrans =
          Set.filter (fn (_, _, r) => SymSet.memb (r, nfa'_accepts)) nfa'_trans
        val updTrans = TranSet.fromList
          (List.concatMap
             (fn (q, x, _) =>
                List.map (fn s => (q, x, s)) (Set.toList nba'_starts))
             (Set.toList filtTrans))
      in
        fromConcr
          { states = SymSet.union (nfa'_states, nba'_states)
          , starts = starts
          , accepts = nba'_accepts
          , trans = TranSet.genUnion [nfa'_trans, nba'_trans, updTrans]
          }
      end
    end

  fun omegaIter (nfa: NFA.nfa) : nba =
    let
      fun renam a =
        Sym.fromTop (Sym.Compound [SOME a])
      val rel = SymRel.mlFunctionToFunction (renam, NFA.states nfa)
      val nfa = NFA.renameStates (nfa, rel)
      val nfa_states = NFA.states nfa
      val nfa_start = NFA.startState nfa
      val nfa_accepts = NFA.acceptingStates nfa
      val nfa_trans = NFA.transitions nfa
      val start' = Sym.fromString "A"
    in
      let
        val filtTrans =
          Set.filter (fn (q, _, _) => Sym.equal (q, nfa_start)) nfa_trans
        val trans' = TranSet.union
          (nfa_trans, TranSet.map (fn (q, x, r) => (start', x, r)) filtTrans)
        val filtTrans' =
          Set.filter (fn (_, _, r) => SymSet.memb (r, nfa_accepts)) trans'
        val trans'' = TranSet.union
          (trans', TranSet.map (fn (q, x, r) => (q, x, start')) filtTrans')
      in
        fromConcr
          { states = SymSet.union (Set.sing start', nfa_states)
          , starts = Set.sing start'
          , accepts = Set.sing start'
          , trans = trans''
          }
      end
    end

  fun fromOmegaReg (or: OmegaReg.omegaReg) : nba =
    case (OmegaReg.toConcr or) of
      OmegaReg.OmegaIter r =>
        omegaIter ((NFA.fromEFA o EFA.fromFA o FA.fromReg) r)
    | OmegaReg.Concat (r, or') =>
        let
          val nfa = (NFA.fromEFA o EFA.fromFA o FA.fromReg) r
          val nba = fromOmegaReg (OmegaReg.fromConcr or')
        in
          concat (nfa, nba)
        end
    | OmegaReg.Union (or1, or2) =>
        let
          val nba1 = fromOmegaReg (OmegaReg.fromConcr or1)
          val nba2 = fromOmegaReg (OmegaReg.fromConcr or2)
        in
          union (nba1, nba2)
        end

end
