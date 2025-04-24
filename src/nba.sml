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
        case SOME (TextIO.openOut fil) handle _ => NONE of
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
