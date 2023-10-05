module Futhark.Optimise.IntraSeq (intraSeq) where

import Futhark.Pass
import Futhark.IR.GPU
import Futhark.Builder.Class

import Control.Monad.Reader
import Control.Monad.State
-- mkLet [Ident (VName (nameFromString "abc") 0) (Prim $ IntType Int32 )] 
--                              (BasicOp $ UnOp (Complement Int32) (Constant $ blankPrimValue (IntType Int32)))

--let stmts = [
--                        mkLet [Ident (VName (nameFromString "abc") 0) (Prim $ IntType Int32 )] 
--                             (BasicOp $ UnOp (Complement Int32) (Constant $ blankPrimValue (IntType Int32)))
--                        ]
--                in let newProg = Prog (OpaqueTypes []) (stmsFromList stmts) []
--                in pure newProg

-- transformProg :: Prog GPU -> PassM (Prog GPU)
  -- transformProg prog =
  --     let (Prog t0 t1 funs) = prog
  --         funs' = map transformFuns funs
  --     in pure $ Prog t0 t1 funs'

  -- transformFuns ::  FunDef GPU -> FunDef GPU
  -- transformFuns fun =
  --     let (FunDef t0 t1 t2 t3 t4 body) = fun
  --         body' = transformBody body
  --     in FunDef t0 t1 t2 t3 t4 body'

  -- transformBody :: Body GPU -> Body GPU
  -- transformBody (Body t0 stmts t1) =
  --     let stmts' = stmsFromList $ map transformStmt (stmsToList stmts)
  --     in Body t0 stmts' t1

  -- transformStmt :: Stm GPU -> Stm GPU
  -- transformStmt _ =
  --     mkLet [Ident (VName (nameFromString "best_stm") 0) (Prim $ IntType Int32 )]
--         (BasicOp $ UnOp (Complement Int32) (Constant $ blankPrimValue (IntType Int32)))

type SeqM = ReaderT (Scope GPU) (State VNameSource)


intraSeq :: Pass GPU GPU
intraSeq =
    Pass "name" "description" $
      intraproceduralTransformation onStms
    where
      onStms scope stms =
        modifyNameSource $
          runState $
            runReaderT (seqStms stms) scope

-- seqStms :: Stms GPU -> SeqM (Stms GPU)
-- seqStms stms =
--   localScope (scopeOf stms) $ do
--     foldM foldfun mempty $ stmsToList stms
  -- where
--     foldfun :: Stms GPU -> Stm GPU -> SeqM (Stms GPU)
--     foldfun ss s = do
--       s' <- seqStm s 
--       pure $ ss <> s'

-- seqStm :: Stm GPU -> SeqM (Stms GPU)
-- seqStm stm@(Let pat aux (Op (SegOp (SegMap lvl@SegGroup {} space ts kbody)))) = do


-- segStm stm = undefined


seqStms :: Stms GPU -> SeqM (Stms GPU)
seqStms stms = do
  stmsMapped <- mapM seqStm $ stmsToList stms
  pure $ stmsFromList stmsMapped

-- We need to somehow now in which segmented operation we are.
-- e.g. when in a SegRed we should produce sequential reduce operations over
-- a chunk using the same operator

seqStm :: Stm GPU -> SeqM (Stm GPU)
-- seqStm (Let pat aux (Op (SegOp segop))) =
--   case segLevel segop of
--     (SegGroup _ _)  -> do
--       kbody' <- seqBody $ segBody segop
--       pure $ case segop of
--               SegMap  lvl space       ts _ -> Let pat aux $ Op $ SegOp $ SegMap lvl space ts kbody'
--               SegRed  lvl space binop ts _ -> Let pat aux $ Op $ SegOp $ SegRed lvl space binop ts kbody'
--               SegScan lvl space binop ts _ -> Let pat aux $ Op $ SegOp $ SegScan lvl space binop ts kbody'
--               SegHist lvl space binop ts _ -> Let pat aux $ Op $ SegOp $ SegHist lvl space binop ts kbody'
--     (SegThread _ _) ->
--       case segop of
--         SegMap {} -> undefined
--         SegRed {} -> undefined
--         SegScan {} -> undefined
--         SegHist {} -> undefined
--     _ -> undefined

seqStm (Let pat aux (Op (SegOp (SegMap lvl space ts kbody)))) =
  case lvl of
    SegGroup {} -> do
      kbody' <- seqBody kbody
      pure $ Let pat aux (Op (SegOp (SegMap lvl space ts kbody')))
    SegThread {} -> undefined
    _ -> undefined

seqStm (Let pat aux (Op (SegOp (SegRed lvl space binops ts kbody)))) =
  case lvl of
    SegGroup {} -> do
      kbody' <- seqBody kbody
      pure $ Let pat aux (Op (SegOp (SegRed lvl space binops ts kbody')))
    SegThread {} -> undefined
    _ -> undefined

seqStm (Let pat aux (Op (SegOp (SegScan lvl space binops ts kbody)))) =
  case lvl of
    SegGroup {} -> do
      kbody' <- seqBody kbody
      pure $ Let pat aux (Op (SegOp (SegScan lvl space binops ts kbody')))
    SegThread {} -> undefined
    _ -> undefined
    
seqStm (Let pat aux (Op (SegOp (SegHist lvl space binops ts kbody)))) =
  case lvl of
    SegGroup {} -> do
      kbody' <- seqBody kbody
      pure $ Let pat aux (Op (SegOp (SegHist lvl space binops ts kbody')))
    SegThread {} -> undefined
    _ -> undefined

    
seqStm (Let pat aux (Op op)) = undefined

-- The catch all base case for now
seqStm _ = undefined




-- 
seqBody :: KernelBody GPU -> SeqM (KernelBody GPU)
seqBody (KernelBody dec stms result) = do
  stms' <- seqStms stms
  pure $ KernelBody dec stms' result


-- Helper functions --

-- segOpType :: SegOp GPU -> 