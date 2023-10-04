module Futhark.Optimise.IntraSeq (intraSeq) where

import Futhark.Pass
import Futhark.IR.GPU
import Futhark.IR.Syntax
import Futhark.Builder.Class
import Futhark.FreshNames
import Futhark.MonadFreshNames

import Control.Monad.Identity
import Control.Monad.State (runState, State)
import Control.Monad.Reader
import qualified Data.Map.Strict as M

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

seqStms :: Stms GPU -> SeqM (Stms GPU)
seqStms stms =
  localScope (scopeOf stms) $ do
    foldM foldfun mempty $ stmsToList stms
  where
    foldfun :: Stms GPU -> Stm GPU -> SeqM (Stms GPU)
    foldfun ss s = do
      s' <- seqStm s 
      pure $ ss <> s'

seqStm :: Stm GPU -> SeqM (Stms GPU)
seqStm stm@(Let pat aux (Op (SegOp (SegMap lvl@SegGroup {} space ts kbody)))) = do
  

segStm stm = undefined
