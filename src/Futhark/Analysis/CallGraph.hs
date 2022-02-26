{-# LANGUAGE OverloadedStrings #-}

-- | This module exports functionality for generating a call graph of
-- an Futhark program.
module Futhark.Analysis.CallGraph
  ( CallGraph,
    buildCallGraph,
    isFunInCallGraph,
    calls,
    calledByConsts,
    allCalledBy,
    numOccurences,
    findNoninlined,
  )
where

import Control.Monad.Writer.Strict
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import qualified Data.Set as S
import Futhark.IR.SOACS
import Futhark.Util.Pretty

type FunctionTable = M.Map Name (FunDef SOACS)

buildFunctionTable :: Prog SOACS -> FunctionTable
buildFunctionTable = foldl expand M.empty . progFuns
  where
    expand ftab f = M.insert (funDefName f) f ftab

-- | A unique (at least within a function) name identifying a function
-- call.  In practice the first element of the corresponding pattern.
type CallId = VName

data FunCalls = FunCalls
  { fcMap :: M.Map CallId (Attrs, Name),
    fcAllCalled :: S.Set Name
  }
  deriving (Eq, Ord, Show)

instance Monoid FunCalls where
  mempty = FunCalls mempty mempty

instance Semigroup FunCalls where
  FunCalls x1 y1 <> FunCalls x2 y2 = FunCalls (x1 <> x2) (y1 <> y2)

fcCalled :: Name -> FunCalls -> Bool
fcCalled f fcs = f `S.member` fcAllCalled fcs

type FunGraph = M.Map Name FunCalls

-- | The call graph is a mapping from a function name, i.e., the
-- caller, to a record of the names of functions called *directly* (not
-- transitively!) by the function.
--
-- We keep track separately of the functions called by constants.
data CallGraph = CallGraph
  { cgCalledByFuns :: FunGraph,
    cgCalledByConsts :: FunCalls
  }
  deriving (Eq, Ord, Show)

-- | Is the given function known to the call graph?
isFunInCallGraph :: Name -> CallGraph -> Bool
isFunInCallGraph f = M.member f . cgCalledByFuns

-- | Does the first function call the second?
calls :: Name -> Name -> CallGraph -> Bool
calls caller callee =
  maybe False (fcCalled callee) . M.lookup caller . cgCalledByFuns

-- | Is the function called in any of the constants?
calledByConsts :: Name -> CallGraph -> Bool
calledByConsts callee = fcCalled callee . cgCalledByConsts

-- | All functions called by this function.
allCalledBy :: Name -> CallGraph -> S.Set Name
allCalledBy f = maybe mempty fcAllCalled . M.lookup f . cgCalledByFuns

-- | @buildCallGraph prog@ build the program's call graph.
buildCallGraph :: Prog SOACS -> CallGraph
buildCallGraph prog =
  CallGraph fg cg
  where
    fg = foldl' (buildFGfun ftable) M.empty entry_points
    cg = buildFGStms $ progConsts prog

    entry_points =
      S.fromList (map funDefName (filter (isJust . funDefEntryPoint) $ progFuns prog))
        <> fcAllCalled cg
    ftable = buildFunctionTable prog

count :: Ord k => [k] -> M.Map k Int
count ks = M.fromListWith (+) $ zip ks $ repeat 1

-- | Produce a mapping of the number of occurences in the call graph
-- of each function.  Only counts functions that are called at least
-- once.
numOccurences :: CallGraph -> M.Map Name Int
numOccurences (CallGraph funs consts) =
  count $ map snd $ M.elems (fcMap consts <> foldMap fcMap (M.elems funs))

-- | @buildCallGraph ftable fg fname@ updates @fg@ with the
-- contributions of function @fname@.
buildFGfun :: FunctionTable -> FunGraph -> Name -> FunGraph
buildFGfun ftable fg fname =
  -- Check if function is a non-builtin that we have not already
  -- processed.
  case M.lookup fname ftable of
    Just f | Nothing <- M.lookup fname fg -> do
      let callees = buildFGBody $ funDefBody f
          fg' = M.insert fname callees fg
      -- recursively build the callees
      foldl' (buildFGfun ftable) fg' $ fcAllCalled callees
    _ -> fg

buildFGStms :: Stms SOACS -> FunCalls
buildFGStms = mconcat . map buildFGstm . stmsToList

buildFGBody :: Body SOACS -> FunCalls
buildFGBody = buildFGStms . bodyStms

buildFGstm :: Stm SOACS -> FunCalls
buildFGstm (Let (Pat (p : _)) aux (Apply fname _ _ _)) =
  FunCalls (M.singleton (patElemName p) (stmAuxAttrs aux, fname)) (S.singleton fname)
buildFGstm (Let _ _ (Op op)) = execWriter $ mapSOACM folder op
  where
    folder =
      identitySOACMapper
        { mapOnSOACLambda = \lam -> do
            tell $ buildFGBody $ lambdaBody lam
            return lam
        }
buildFGstm (Let _ _ e) = execWriter $ mapExpM folder e
  where
    folder =
      identityMapper
        { mapOnBody = \_ body -> do
            tell $ buildFGBody body
            return body
        }

-- | The set of all functions that are called noinline somewhere, or
-- have a noinline attribute on their definition.
findNoninlined :: Prog SOACS -> S.Set Name
findNoninlined prog =
  foldMap noinlineDef (progFuns prog)
    <> foldMap onStm (foldMap (bodyStms . funDefBody) (progFuns prog) <> progConsts prog)
  where
    onStm :: Stm SOACS -> S.Set Name
    onStm (Let _ aux (Apply fname _ _ _))
      | "noinline" `inAttrs` stmAuxAttrs aux =
        S.singleton fname
    onStm (Let _ _ e) = execWriter $ mapExpM folder e
      where
        folder =
          identityMapper
            { mapOnBody = \_ body -> do
                tell $ foldMap onStm $ bodyStms body
                return body,
              mapOnOp =
                mapSOACM
                  identitySOACMapper
                    { mapOnSOACLambda = \lam -> do
                        tell $ foldMap onStm $ bodyStms $ lambdaBody lam
                        return lam
                    }
            }

    noinlineDef fd
      | "noinline" `inAttrs` funDefAttrs fd =
        S.singleton $ funDefName fd
      | otherwise =
        mempty

instance Pretty FunCalls where
  ppr = stack . map f . M.toList . fcMap
    where
      f (x, (attrs, y)) = "=>" <+> ppr y <+> parens ("at" <+> ppr x <+> ppr attrs)

instance Pretty CallGraph where
  ppr (CallGraph fg cg) =
    stack $
      punctuate line $
        ppFunCalls ("called at top level", cg) : map ppFunCalls (M.toList fg)
    where
      ppFunCalls (f, fcalls) =
        ppr f </> text (map (const '=') (nameToString f))
          </> indent 2 (ppr fcalls)
