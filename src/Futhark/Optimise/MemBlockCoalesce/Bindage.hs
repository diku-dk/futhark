{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Playground for work on merging memory blocks
module Futhark.Optimise.MemBlockCoalesce.Bindage (inPlaceUpdates, inPlaceUpdatesAction, InPlaceUpdates) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace
import Futhark.Analysis.Alias
import Futhark.IR.Aliases
import Futhark.IR.Prop.Names
import Futhark.IR.Prop.Patterns
import Futhark.IR.SeqMem (SeqMem)
import qualified Futhark.IR.SeqMem as SeqMem
import Futhark.IR.Syntax
import Futhark.Pipeline

type InPlaceUpdates = Map VName (VName, Slice SubExp)

stmInPlaceUpdates :: Stm (Aliases SeqMem) -> InPlaceUpdates
stmInPlaceUpdates stm@(Let (Pattern _ [patElem]) _ (BasicOp (Update vname slice se))) =
  trace ("pat: " ++ pretty patElem ++ "\nvname: " ++ pretty vname ++ "\nslice: " ++ pretty slice ++ "\nse: " ++ pretty se) $
    Map.singleton (patElemName patElem) (vname, slice)
stmInPlaceUpdates stm@(Let _ _ (DoLoop _ _ _ body)) =
  foldMap stmInPlaceUpdates $ bodyStms body
stmInPlaceUpdates stm@(Let _ _ (If _ then_body else_body _)) =
  (<>)
    (foldMap stmInPlaceUpdates $ bodyStms then_body)
    (foldMap stmInPlaceUpdates $ bodyStms else_body)
stmInPlaceUpdates _ = mempty

inPlaceUpdates :: Prog (Aliases SeqMem) -> InPlaceUpdates
inPlaceUpdates =
  foldMap (foldMap stmInPlaceUpdates . bodyStms . funDefBody) . progFuns

inPlaceUpdatesAction :: Action SeqMem
inPlaceUpdatesAction =
  Action
    { actionName = "InPlaceUpdates",
      actionDescription = "InPlaceUpdates",
      actionProcedure = liftIO . putStrLn . pretty . inPlaceUpdates . aliasAnalysis
    }
