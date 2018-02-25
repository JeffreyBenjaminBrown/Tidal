This parsing strategy is complex, because terms in the parser exhibit hysteresis. In "g2 s3", the g3 continues into the s3; it is equivalent to "g2 g2,s3". Since there are also other kinds of terms like brackets, the parser parses two kinds of `Lexemes`: `LexemeNonEpic`, which include operators and brackets; and `LexemeEpic`, which include the terms that `Epics` are made of (e.g. parameters (in the case of `ParamEpic`s), transforms (in the case of `Epic (Transform a)`s), etc.).

`pLexemeEpic` creates a `LexemeEpic`, which is just a list of phonemes:
```
pLexemeEpic :: Monoidoid i o => Parser (EpicPhoneme o) -> Parser (Lexeme i o)
pLexemeEpic p = lexeme $ LexemeEpic <$> sepBy1 p (some $ char ',')
```

`pLexemes` creates a list of `Lexemes`. It reads the `LexemeEpic`s in various ways depending on its first argument, but reads every `LexemeNonEpic` the same way.
```
pLexemes :: Monoidoid i o => Parser (Lexeme i o) -> Parser [Lexeme i o]
pLexemes p = some $ try p <|> try pLexemeNonEpicLexeme
```

`pLang` transforms `Lexeme`s into `Lang`s. Each `LexemeEpic`, which contains a number of `EpicPhonemes`, has those phonemes merged into a single `AccumEpic`.
```
pLang :: (Monoidoid i o) => Parser [Lexeme i o] -> Parser [Lang i o]
pLang p = map f <$> p where
  f c = case c of
    LexemeEpic list -> LangEpic $ lexemeToAccumEpic list
    LexemeNonEpic nonEpic -> LangNonEpic nonEpic
```

Each `AccumEpic` in the list produced by `pLang` depends on the earlier ones for meaning. (TODO : This might be ignorable, for certain kinds of payloads. Certainly not for `ParamMap`s, but maybe for other ones.)

`pEpicOrOps` turns each `Lang` into an `EpicOrOp`. The main trick here is that it uses `scanLang` to turn each `AccumEpic` into a meaningful-on-its-own `EpicOrOp`.
```
pEpicOrOps :: (Monoidoid i o) =>
  Parser [Lang i o] -> (Time -> i -> Epic i) -> Parser [EpicOrOp i]
pEpicOrOps p loopx = scanLang loopx <$> p
```

Finally, `_p` turns a list of `EpicOrOp`s into an `Epic`. It uses `parseEpicExpr`, which uses `Text.Megaparsec.Expr`.
```
_p :: ((Time -> i -> Epic i) -> Parser [EpicOrOp i])
   -> (Time -> i -> Epic i) -> String -> Epic i
_p p loopx s = case parse (sc >> p loopx) "" s of
  Left e -> error $ show e
  Right r -> case parse parseEpicExpr "" r of
    Left e -> error "unshowable Epic ParamMap parse error"
    Right r -> r
```
