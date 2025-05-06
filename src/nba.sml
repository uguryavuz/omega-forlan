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

  fun alphabet (nba: nba) =
    SymSet.genUnion
      (Set.mapToList (fn (_, bs, _) => SymSet.fromList bs) (transitions nba))

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
      val _ =
        if (NFA.accepted nfa (Str.fromString "%")) then
          M.errorPP (fn () =>
            [ PP.fromString "NFA"
            , PP.fromString "must"
            , PP.fromString "not"
            , PP.fromString "accept"
            , PP.fromString "the"
            , PP.fromString "empty"
            , PP.fromString "string"
            ])
        else
          ()
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
        omegaIter
          ((DFA.injToNFA o DFA.minimize o DFA.fromNFA o NFA.fromEFA o EFA.fromFA
            o FA.fromReg) r)
    | OmegaReg.Concat (r, or') =>
        let
          val nfa =
            (DFA.injToNFA o DFA.minimize o DFA.fromNFA o NFA.fromEFA
             o EFA.fromFA o FA.fromReg) r
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

  fun toStartAccRegPairs (nba: nba) : (Reg.reg * Reg.reg) list =
    let
      val states = #states nba
      val starts = #starts nba
      val accepts = #accepts nba
      fun statePairFA (s, s') =
        FA.fromConcr
          { stats = states
          , start = s
          , accepting = Set.sing s'
          , trans = #trans nba
          }
      fun statePairReg (s, s') =
        RFA.faToReg Reg.weaklySimplify (statePairFA (s, s'))
      fun statePairFASE (s, s') =
        if not (Sym.equal (s, s')) then
          statePairFA (s, s')
        else
          let
            fun renam a =
              Sym.fromTop (Sym.Compound [SOME a])
            val rel = SymRel.mlFunctionToFunction (renam, states)
            val fa = FA.renameStates (statePairFA (s, s'), rel)
            val fa_states = FA.states fa
            val fa_start = FA.startState fa
            val fa_trans = FA.transitions fa
            val start' = Sym.fromString "A"
          in
            let
              val filtTrans =
                Set.filter (fn (q, _, _) => Sym.equal (q, fa_start)) fa_trans
              val trans' = TranSet.union
                ( fa_trans
                , TranSet.map (fn (q, x, r) => (start', x, r)) filtTrans
                )
            in
              FA.fromConcr
                { stats = SymSet.union (Set.sing start', fa_states)
                , start = start'
                , accepting = FA.acceptingStates fa
                , trans = trans'
                }
            end
          end
      fun statePairRegSE (s, s') =
        RFA.faToReg Reg.weaklySimplify (statePairFASE (s, s'))
      val startAccPairs = Set.toList (Set.times (starts, accepts))
    in
      List.map
        (fn (s, s') =>
           let
             val r1 = statePairReg (s, s')
             val r2 = statePairRegSE (s', s')
           in
             (r1, r2)
           end) startAccPairs
    end

  fun toOmegaReg (nba: nba) : OmegaReg.omegaReg =
    OmegaReg.fromFinUnionPairs (toStartAccRegPairs nba)

  fun isEmpty (nba: nba) : bool =
    let
      fun lassoPair (r1, r2) =
        not (Reg.isEmptySet r1) andalso not (Reg.isEmptySet r2)
    in
      not (List.exists lassoPair (toStartAccRegPairs nba))
    end

  fun inter (nba1: nba, nba2: nba) : nba =
    let
      val states1 = #states nba1
      val states2 = #states nba2
      val starts1 = #starts nba1
      val starts2 = #starts nba2
      val accepts1 = #accepts nba1
      val accepts2 = #accepts nba2
      val transList1 = Set.toList (#trans nba1)
      val transList2 = Set.toList (#trans nba2)
      val symb1 = Sym.fromString "1"
      val symb2 = Sym.fromString "2"
      val rawStates = Set.times3 (states1, states2, SymSet.fromString "1,2")
      val rawStarts = Set.times3 (starts1, starts2, Set.sing symb1)
      val rawAccepts = Set.times3 (states1, accepts2, Set.sing symb2)
      fun edgesForTransPair (s1, x1, s1') (s2, x2, s2') :
        ((Sym.sym * Sym.sym * Sym.sym) * Str.str * (Sym.sym * Sym.sym * Sym.sym)) list =
        if Str.equal (x1, x2) then
          let
            val firstEdge =
              if SymSet.memb (s1, accepts1) then
                ((s1, s2, symb1), x1, (s1', s2', symb2))
              else
                ((s1, s2, symb1), x1, (s1', s2', symb1))
            val secondEdge =
              if SymSet.memb (s2, accepts2) then
                ((s1, s2, symb2), x1, (s1', s2', symb1))
              else
                ((s1, s2, symb2), x1, (s1', s2', symb2))
          in
            [firstEdge, secondEdge]
          end
        else
          []
      fun edgesForTransFrom1 (s1, x1, s1') =
        List.concatMap
          (fn (s2, x2, s2') => edgesForTransPair (s1, x1, s1') (s2, x2, s2'))
          transList2
      val rawTransList = List.concatMap edgesForTransFrom1 transList1
      fun tripleToSym (s1, s2, symb) =
        Sym.fromString
          ("<" ^ Sym.toString s1 ^ ", " ^ Sym.toString s2 ^ ", "
           ^ Sym.toString symb ^ ">")
    in
      fromConcr
        { states = SymSet.map tripleToSym rawStates
        , starts = SymSet.map tripleToSym rawStarts
        , accepts = SymSet.map tripleToSym rawAccepts
        , trans = TranSet.fromList
            (List.map
               (fn ((s1, s2, symb), x, (s1', s2', symb')) =>
                  let
                    val first = tripleToSym (s1, s2, symb)
                    val second = tripleToSym (s1', s2', symb')
                  in
                    (first, x, second)
                  end) rawTransList)
        }
    end

  fun complement (nba: nba, alph: Sym.sym Set.set, verbose: bool) =
    let
      val states = #states nba
      val starts = #starts nba
      val accepts = #accepts nba
      val trans = #trans nba
      val statePowersetAsList: (Sym.sym Set.set) list =
        let
          val elems = Set.toList states
          fun ps [] = [[]]
            | ps (x :: xs) =
                let val rest = ps xs
                in rest @ (map (fn ys => x :: ys) rest)
                end
        in
          map (fn lst => SymSet.fromList lst) (ps elems)
        end
      val allLevelRankings: (Sym.sym * int) list vector =
        let
          val nonAcceptChoices =
            List.tabulate (2 * (Set.size states) + 1, fn i => i)
          val acceptChoices = List.tabulate ((Set.size states) + 1, fn i =>
            i * 2)
          fun expandRankingDom x c lrsList =
            List.map (fn lr => (x, c) :: lr) lrsList
          fun expandRankingDomOnChoiceList x choices lrsList =
            List.concatMap (fn c => expandRankingDom x c lrsList) choices
          fun expandRankingDomForStateWithAllChoices x lrsList =
            if SymSet.memb (x, accepts) then
              expandRankingDomOnChoiceList x acceptChoices lrsList
            else
              expandRankingDomOnChoiceList x nonAcceptChoices lrsList
          fun allRankingsFromListOfStates stateList =
            case stateList of
              [] => [[]]
            | x :: xs =>
                expandRankingDomForStateWithAllChoices x
                  (allRankingsFromListOfStates xs)
        in
          Vector.fromList (allRankingsFromListOfStates (Set.toList states))
        end
      val allLevelRankingsIndices: int list =
        List.tabulate (Vector.length allLevelRankings, fn i => i)
      val allLevelRankingsFns: (Sym.sym -> int) vector =
        Vector.map
          (fn ranking =>
             fn x =>
               case List.find (fn (y, _) => Sym.equal (y, x)) ranking of
                 SOME (_, v) => v
               | NONE => raise Fail "Key not found") allLevelRankings
      val allTightLevelRankingsIndices: int list =
        List.filter
          (fn i =>
             let
               val ranking = Vector.sub (allLevelRankings, i)
               val max_rank =
                 List.foldl (fn ((_, r), acc) => Int.max (r, acc)) 0 ranking
               fun odds_upto n : int list =
                 List.tabulate (n div 2 + n mod 2, fn i => 2 * i + 1)
             in
               if (max_rank mod 2 = 0) then
                 false
               else
                 List.all (fn r => (List.exists (fn (_, r') => r = r') ranking))
                   (odds_upto max_rank)
             end) allLevelRankingsIndices
      fun allSetSigmaCoveringTightLevelRankingsIndices (x: Str.str)
        (P: Sym.sym Set.set) (i: int) : int list =
        let
          val rankingFn_g = Vector.sub (allLevelRankingsFns, i)
        in
          List.filter
            (fn j =>
               let
                 val rankingFn_g' = Vector.sub (allLevelRankingsFns, j)
               in
                 List.all
                   (fn (q, x', q') =>
                      if
                        (not (Str.equal (x, x')))
                        orelse (not (SymSet.memb (q, P)))
                      then true
                      else rankingFn_g' q' <= rankingFn_g q) (Set.toList trans)
               end) allTightLevelRankingsIndices
        end
      fun evenStatesOfRanking (i: int) : Sym.sym Set.set =
        let
          val ranking_g = Vector.sub (allLevelRankings, i)
          val evenPairs = List.filter (fn (_, v) => v mod 2 = 0) ranking_g
        in
          SymSet.fromList (List.map (fn (s, _) => s) evenPairs)
        end
      fun neighborsThroughSigma (x: Str.str) (P: Sym.sym Set.set) :
        Sym.sym Set.set =
        let
          val neighbors =
            List.mapPartial
              (fn (q, x', q') =>
                 if SymSet.memb (q, P) andalso Str.equal (x, x') then SOME q'
                 else NONE) (Set.toList trans)
        in
          SymSet.fromList neighbors
        end
      fun adjacentTriplesThroughSigma (x: Str.str)
        (S: Sym.sym Set.set, O: Sym.sym Set.set, i: int) =
        let
          val setSigmaCoveringTightLevelRankingsIndices =
            allSetSigmaCoveringTightLevelRankingsIndices x S i
          fun tripleGivenLevelRankingIdx (j: int) =
            let
              val evenStatesOf_j = evenStatesOfRanking j
              val firstSet = neighborsThroughSigma x S
              val secondSet =
                if (Set.isEmpty O) then SymSet.inter (firstSet, evenStatesOf_j)
                else SymSet.inter (neighborsThroughSigma x O, evenStatesOf_j)
            in
              (firstSet, secondSet, j)
            end
        in
          List.map (fn j => tripleGivenLevelRankingIdx j)
            setSigmaCoveringTightLevelRankingsIndices
        end
      val ordOnTriples =
        Set.compareTriple (SymSet.compare, SymSet.compare, Int.compare)
      fun tripleToSym (S, O, i) =
        let
          val s =
            "<<" ^ SymSet.toString S ^ ">, <" ^ SymSet.toString O ^ ">, "
            ^ Int.toString i ^ ">"
        in
          Sym.fromString s
        end
      val initialSet = Set.times3
        ( Set.sing starts
        , Set.sing Set.empty
        , Set.fromList Int.compare allTightLevelRankingsIndices
        )
      val buildStatesAndTrans =
        let
          val sigmaStrList: Str.str list =
            List.map (fn x => [x]) (Set.toList alph)
          val analyzed = ref (Set.empty)
          val seen = ref (initialSet)
          val worklist = ref (Set.toList initialSet)
          val triples = ref []
          fun successorsThroughSigma x (S, O, i) =
            adjacentTriplesThroughSigma x (S, O, i)
          fun step () =
            case (!worklist) of
              [] => ()
            | (S, O, i) :: rest =>
                let
                  val _ = worklist := rest
                in
                  if Set.memb ordOnTriples ((S, O, i), !analyzed) then
                    ()
                  else
                    let
                      val _ =
                        analyzed
                        :=
                        Set.union ordOnTriples (!analyzed, Set.sing ((S, O, i)))
                      val _ =
                        List.app
                          (fn x =>
                             List.app
                               (fn (S', O', j) =>
                                  let
                                    val _ =
                                      triples
                                      :=
                                      ((S, O, i), x, (S', O', j)) :: (!triples)
                                    val _ =
                                      if
                                        (Set.memb ordOnTriples
                                           ((S', O', j), !seen))
                                      then
                                        ()
                                      else
                                        ( worklist := (S', O', j) :: (!worklist)
                                        ; seen
                                          :=
                                          Set.union ordOnTriples
                                            (!seen, Set.sing ((S', O', j)))
                                        )
                                  in
                                    ()
                                  end) (successorsThroughSigma x (S, O, i)))
                          sigmaStrList
                    in
                      ()
                    end
                end
          fun loop () =
            if null (!worklist) then
              (!analyzed, !triples)
            else
              ( (let
                   val _ =
                     if verbose then
                       (print
                          ("Analyzing state: "
                           ^ (Sym.toString o tripleToSym) (hd (!worklist))
                           ^ "\n"))
                     else
                       ()
                 in
                   ( step ()
                   ; let
                       val _ =
                         if verbose then
                           (print
                              ("-> Analyzed states: "
                               ^ (Int.toString o Set.size) (!analyzed) ^ "\n"))
                         else
                           ()
                       val _ =
                         if verbose then
                           (print
                              ("   Seen states: "
                               ^ (Int.toString o Set.size) (!seen) ^ "\n"))
                         else
                           ()
                       val _ =
                         if verbose then
                           (print
                              ("   Worklist length: "
                               ^ (Int.toString o List.length) (!worklist) ^ "\n"))
                         else
                           ()
                       val _ =
                         if verbose then
                           (print
                              ("   Transition count: "
                               ^ (Int.toString o List.length) (!triples) ^ "\n"))
                         else
                           ()
                     in
                       ()
                     end
                   )
                 end)
              ; loop ()
              )
        in
          loop ()
        end
    in
      let
        val (rawStates, rawTrans) = buildStatesAndTrans
        val rawAccepts = Set.filter (fn (_, O, _) => Set.isEmpty O) rawStates
        val states' = SymSet.map tripleToSym rawStates
        val accepts' = SymSet.map tripleToSym rawAccepts
        val trans' = TranSet.fromList
          (List.map
             (fn ((S, O, i), x, (S', O', j)) =>
                let
                  val first = tripleToSym (S, O, i)
                  val second = tripleToSym (S', O', j)
                in
                  (first, x, second)
                end) rawTrans)
        val _ =
          if verbose then
            (print ("Total states: " ^ (Int.toString o Set.size) states' ^ "\n"))
          else
            ()
        val _ =
          if verbose then
            (print
               ("Total transitions: " ^ (Int.toString o Set.size) trans' ^ "\n"))
          else
            ()
      in
        fromConcr
          { states = states'
          , starts = SymSet.map tripleToSym initialSet
          , accepts = accepts'
          , trans = trans'
          }
      end
    end

  fun equivalent (nba1, nba2) : bool =
    let
      val alphUnion = SymSet.union (alphabet nba1, alphabet nba2)
      val b1 = inter (nba1, complement (nba2, alphUnion, false))
      val b2 = inter (nba2, complement (nba1, alphUnion, false))
    in
      (isEmpty b1 andalso isEmpty b2)
    end

  fun isUniversal (nba: nba, alph: Sym.sym Set.set) : bool =
    isEmpty (complement (nba, alph, false))

end
