structure CustomLex :> CUSTOM_LEX =
struct
  datatype tok =
    Bar
  | Comma
  | Dollar
  | Perc
  | Plus
  | Semicolon
  | Star
  | Tilde
  | OpenPar
  | ClosPar
  | OpenBrack
  | ClosBrack
  | At
  | SingArr
  | DoubArr
  | Sym of Lex.sym
  | Heading of string (* string should have no whitespace *)
  | EOF

  fun customTokToForlanTok tok =
    case tok of
      Bar => Lex.Bar
    | Comma => Lex.Comma
    | Dollar => Lex.Dollar
    | Perc => Lex.Perc
    | Plus => Lex.Plus
    | Semicolon => Lex.Semicolon
    | Star => Lex.Star
    | Tilde => Lex.Tilde
    | OpenPar => Lex.OpenPar
    | ClosPar => Lex.ClosPar
    | SingArr => Lex.SingArr
    | DoubArr => Lex.DoubArr
    | Sym s => Lex.Sym s
    | Heading s => Lex.Heading s
    | EOF => Lex.EOF
    | _ => Messages.cannotHappen ()

  fun equalTok (Bar, Bar) = true
    | equalTok (Comma, Comma) = true
    | equalTok (Dollar, Dollar) = true
    | equalTok (Perc, Perc) = true
    | equalTok (Plus, Plus) = true
    | equalTok (Semicolon, Semicolon) = true
    | equalTok (Star, Star) = true
    | equalTok (Tilde, Tilde) = true
    | equalTok (OpenPar, OpenPar) = true
    | equalTok (ClosPar, ClosPar) = true
    | equalTok (OpenBrack, OpenBrack) = true
    | equalTok (ClosBrack, ClosBrack) = true
    | equalTok (At, At) = true
    | equalTok (SingArr, SingArr) = true
    | equalTok (DoubArr, DoubArr) = true
    | equalTok (Sym a, Sym b) =
        Lex.compareSym (a, b) = EQUAL
    | equalTok (Heading x, Heading y) = x = y
    | equalTok (EOF, EOF) = true
    | equalTok _ = false

  fun tokToMsg Bar =
        PP.quote (PP.fromString "|")
    | tokToMsg Comma =
        PP.quote (PP.fromString ",")
    | tokToMsg Dollar =
        PP.quote (PP.fromString "$")
    | tokToMsg Perc =
        PP.quote (PP.fromString "%")
    | tokToMsg Plus =
        PP.quote (PP.fromString "+")
    | tokToMsg Semicolon =
        PP.quote (PP.fromString ";")
    | tokToMsg Star =
        PP.quote (PP.fromString "*")
    | tokToMsg Tilde =
        PP.quote (PP.fromString "~")
    | tokToMsg OpenPar =
        PP.quote (PP.fromString "(")
    | tokToMsg ClosPar =
        PP.quote (PP.fromString ")")
    | tokToMsg OpenBrack =
        PP.quote (PP.fromString "[")
    | tokToMsg ClosBrack =
        PP.quote (PP.fromString "]")
    | tokToMsg At =
        PP.quote (PP.fromString "@")
    | tokToMsg SingArr =
        PP.quote (PP.fromString "->")
    | tokToMsg DoubArr =
        PP.quote (PP.fromString "=>")
    | tokToMsg (Sym a) =
        PP.quote (Lex.symToPP a)
    | tokToMsg (Heading s) =
        PP.quote (PP.fromString s)
    | tokToMsg EOF = PP.fromString "end-of-file"

  fun errorNotEOFTerminated () =
    Messages.errorString (fn () =>
      ["labeled", "token", "list", "isn't", "EOF-terminated"])

  fun expectedTok (n, t) =
    Messages.errorPP (fn () =>
      [ PP.fromString "line"
      , PP.colon (PP.fromString (Int.toString n))
      , tokToMsg t
      , PP.fromString "expected"
      ])

  fun expectedDigit n =
    Messages.errorPP (fn () =>
      [ PP.fromString "line"
      , PP.colon (PP.fromString (Int.toString n))
      , PP.fromString "digit"
      , PP.fromString "expected"
      ])

  fun expectedLetter n =
    Messages.errorPP (fn () =>
      [ PP.fromString "line"
      , PP.colon (PP.fromString (Int.toString n))
      , PP.fromString "letter"
      , PP.fromString "expected"
      ])

  fun expectedLetterOrDigit n =
    Messages.errorPP (fn () =>
      [ PP.fromString "line"
      , PP.colon (PP.fromString (Int.toString n))
      , PP.fromString "letter"
      , PP.fromString "or"
      , PP.fromString "digit"
      , PP.fromString "expected"
      ])

  fun unexpectedTok (n, t) =
    Messages.errorPP (fn () =>
      [ PP.fromString "line"
      , PP.colon (PP.fromString (Int.toString n))
      , tokToMsg t
      , PP.fromString "unexpected"
      ])

  fun checkInLabToks (_, nil) = errorNotEOFTerminated ()
    | checkInLabToks (t, ((n, u) :: lts)) =
        if equalTok (u, t) then lts else expectedTok (n, t)

  fun error (begLin, endLin, pps) =
    if begLin = endLin then
      Messages.errorPP (fn () =>
        [PP.fromString "line", PP.colon (PP.fromString (Int.toString begLin))]
        @ pps)
    else
      Messages.errorPP (fn () =>
        [ PP.fromString "lines"
        , PP.fromString (Int.toString begLin)
        , PP.fromString "-"
        , PP.colon (PP.fromString (Int.toString endLin))
        ] @ pps)

  fun initState (lin, nil, lts) =
        rev ((lin, EOF) :: lts)
    | initState (lin, c :: cs, lts) =
        case c of
          #"|" => initState (lin, cs, (lin, Bar) :: lts)
        | #"," => initState (lin, cs, (lin, Comma) :: lts)
        | #"$" => initState (lin, cs, (lin, Dollar) :: lts)
        | #"%" => initState (lin, cs, (lin, Perc) :: lts)
        | #"+" => initState (lin, cs, (lin, Plus) :: lts)
        | #";" => initState (lin, cs, (lin, Semicolon) :: lts)
        | #"*" => initState (lin, cs, (lin, Star) :: lts)
        | #"~" => initState (lin, cs, (lin, Tilde) :: lts)
        | #"(" => initState (lin, cs, (lin, OpenPar) :: lts)
        | #")" => initState (lin, cs, (lin, ClosPar) :: lts)
        | #"[" => initState (lin, cs, (lin, OpenBrack) :: lts)
        | #"]" => initState (lin, cs, (lin, ClosBrack) :: lts)
        | #"@" => initState (lin, cs, (lin, At) :: lts)
        | #"-" => singArrState (lin, cs, lts, lin)
        | #"=" => doubArrState (lin, cs, lts, lin)
        | #"<" => symState (lin, cs, lts, lin, "<", nil, nil)
        | #"{" => headingState (lin, cs, lts, lin, "{")
        | #"#" => commentState (lin, cs, lts)
        | #"\n" => initState (lin + 1, cs, lts)
        | _ =>
            if Char.isAlphaNum c then
              initState
                ( lin
                , cs
                , ( lin
                  , Sym (Lex.symTopToSym (Lex.BasicSymTop (Lex.charToBasic c)))
                  ) :: lts
                )
            else if Char.isSpace c then
              initState (lin, cs, lts)
            else
              error
                ( lin
                , lin
                , [ PP.fromString "unexpected"
                  , PP.fromString "character:"
                  , PP.quote (PP.fromString (Char.toString c))
                  ]
                )

  and singArrState (lin, nil, _, beg) =
        error
          ( beg
          , lin
          , [PP.quote (PP.fromString "->"), PP.fromString "unfinished"]
          )
    | singArrState (lin, c :: cs, lts, beg) =
        if c = #">" then
          initState (lin, cs, (beg, SingArr) :: lts)
        else if c = #"\n" then
          singArrState (lin + 1, cs, lts, beg)
        else if Char.isSpace c then
          singArrState (lin, cs, lts, beg)
        else
          error
            ( beg
            , lin
            , [PP.quote (PP.fromString "->"), PP.fromString "unfinished"]
            )

  and doubArrState (lin, nil, _, beg) =
        error
          ( beg
          , lin
          , [PP.quote (PP.fromString "=>"), PP.fromString "unfinished"]
          )
    | doubArrState (lin, c :: cs, lts, beg) =
        if c = #">" then
          initState (lin, cs, (beg, DoubArr) :: lts)
        else if c = #"\n" then
          doubArrState (lin + 1, cs, lts, beg)
        else if Char.isSpace c then
          doubArrState (lin, cs, lts, beg)
        else
          error
            ( beg
            , lin
            , [PP.quote (PP.fromString "=>"), PP.fromString "unfinished"]
            )

  and symState (lin, nil, _, beg, s, _, _) =
        error
          ( beg
          , lin
          , [ PP.fromString "bad"
            , PP.fromString "symbol:"
            , PP.quote (PP.fromStringSplitEscape s)
            ]
          )
    | symState (lin, c :: cs, lts, beg, s, xs, yss) =
        if c = #"<" then
          symState (lin, cs, lts, beg, s ^ "<", nil, xs :: yss)
        else if c = #">" then
          if List.null yss then
            initState
              ( lin
              , cs
              , (beg, Sym (Lex.symTopToSym (Lex.CompoundSymTop (List.rev xs))))
                :: lts
              )
          else
            symState
              ( lin
              , cs
              , lts
              , beg
              , s ^ ">"
              , SOME (Lex.symTopToSym (Lex.CompoundSymTop (List.rev xs)))
                :: hd yss
              , tl yss
              )
        else if c = #"\n" then
          symState (lin + 1, cs, lts, beg, s, xs, yss)
        else if Char.isSpace c then
          symState (lin, cs, lts, beg, s, xs, yss)
        else if Char.isAlphaNum c then
          symState
            ( lin
            , cs
            , lts
            , beg
            , s ^ str c
            , SOME (Lex.symTopToSym (Lex.BasicSymTop (Lex.charToBasic c))) :: xs
            , yss
            )
        else if c = #"," then
          symState (lin, cs, lts, beg, s ^ ",", NONE :: xs, yss)
        else
          error
            ( beg
            , lin
            , [ PP.fromString "bad"
              , PP.fromString "symbol"
              , PP.fromString "prefix:"
              , PP.quote (PP.fromStringSplitEscape (s ^ str c))
              ]
            )

  and headingState (lin, nil, _, beg, s) =
        error
          ( beg
          , lin
          , [ PP.fromString "bad"
            , PP.fromString "heading:"
            , PP.quote (PP.fromStringSplitEscape s)
            ]
          )
    | headingState (lin, c :: cs, lts, beg, s) =
        if c = #"}" then
          initState (lin, cs, (beg, Heading (s ^ "}")) :: lts)
        else if c = #"\n" then
          headingState (lin + 1, cs, lts, beg, s)
        else if Char.isSpace c then
          headingState (lin, cs, lts, beg, s)
        else if Char.isAlpha c then
          headingState (lin, cs, lts, beg, s ^ str (Char.toLower c))
        else
          error
            ( beg
            , lin
            , [ PP.fromString "bad"
              , PP.fromString "heading"
              , PP.fromString "prefix:"
              , PP.quote (PP.fromStringSplitEscape (s ^ str c))
              ]
            )

  and commentState (lin, nil, lts) =
        rev ((lin, EOF) :: lts)
    | commentState (lin, c :: cs, lts) =
        if c = #"\n" then initState (lin + 1, cs, lts)
        else commentState (lin, cs, lts)

  fun lexString s =
    initState (1, explode s, nil)

  fun lexFile fil =
    let
      fun findErr fil =
        Messages.errorPP (fn () =>
          [ PP.fromString "file"
          , PP.quote (PP.fromStringSplitEscape fil)
          , PP.fromString "not"
          , PP.fromString "found"
          ])

      fun openErr fil =
        Messages.errorPP (fn () =>
          [ PP.fromString "unable"
          , PP.fromString "to"
          , PP.fromString "open"
          , PP.fromString "file:"
          , PP.quote (PP.fromStringSplitEscape fil)
          ])

      fun existsFile fil = OS.FileSys.access (fil, nil)

      fun findFile fil =
        let
          val path = Params.getSearchPath ()

          fun fnd nil = findErr fil
            | fnd (dir :: dirs) =
                let val dirFil = OS.Path.concat (dir, fil)
                in if existsFile dirFil then dirFil else fnd dirs
                end
        in
          if existsFile fil then fil
          else if OS.Path.isAbsolute fil then findErr fil
          else fnd path
        end

      fun inputFromStdIn () =
        ( print "@ "
        ; case TextIO.inputLine TextIO.stdIn of
            NONE =>
              Messages.errorString (fn () =>
                ["input", "incorrectly", "terminated"])
          | SOME ".\n" => ""
          | SOME s => s ^ inputFromStdIn ()
        )
    in
      if fil = "" then
        initState (1, explode (inputFromStdIn ()), nil)
      else
        let
          val fil = findFile fil
          val stm = TextIO.openIn fil handle _ => openErr fil
          val lts = initState (1, explode (TextIO.inputAll stm), nil)
        in
          TextIO.closeIn stm;
          lts
        end
    end

end;
