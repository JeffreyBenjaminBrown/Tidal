data SeqExpr = SeqExprEpic (Epic ParamMap)
             | forall a. SeqExprOp1 (Epic a -> Epic a)
             | forall a. SeqExprOp2 (Epic a -> Epic a -> Epic a)
  maybe make precedence part of those operator constructors

change _scanAccumBlocks:
  It has been :: (Dur, ParamMap) -> [CmdBlock] -> [(Dur, ParamMap)].
