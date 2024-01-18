module Futhark.Analysis.IndexFn (mkIndexFnProg) where

import Data.Map.Strict qualified as M
import Futhark.Analysis.Refinement.Monad
import Futhark.MonadFreshNames
import Futhark.Util.Pretty
import Language.Futhark qualified as E
import Language.Futhark.Semantic

import Debug.Trace (trace, traceM)

type IndexFn = Int

mkIndexFnProg :: VNameSource -> Imports -> M.Map E.VName IndexFn
mkIndexFnProg _vnsource = mkIndexFnImports

mkIndexFnImports :: [(ImportName, FileModule)] -> M.Map E.VName IndexFn
mkIndexFnImports = mconcat . map (mkIndexFnDecs . E.progDecs . fileProg . snd)
-- A program is a list of declarations (DecBase); functions are value bindings
-- (ValBind).

mkIndexFnDecs :: [E.Dec] -> M.Map E.VName IndexFn
mkIndexFnDecs [] = M.empty
mkIndexFnDecs (E.ValDec vb : rest) =
  mkIndexFnValBind vb `M.union` mkIndexFnDecs rest
mkIndexFnDecs (_ : ds) = mkIndexFnDecs ds

mkIndexFnValBind :: E.ValBind -> M.Map E.VName IndexFn
mkIndexFnValBind (E.ValBind _ vn ret _ _ params body _ _ _) =
  -- mapM_ paramRefs params
  -- forwards body
  case ret of
    Just (E.TERefine t p _) -> do
      trace ("\n====\nmkIndexFnValBind: "
             <> prettyString vn
             <> "\n\nTo prove\n--------\n"
             <> prettyString ret)
        $ trace ("\nFor body\n--------\n" <> prettyString body <> "\n====\n")
        $ trace ("t: " <> prettyString t)
        -- $ trace ("body: " <> show (body))
        $ trace ("getRes: " <> prettyString (getRes body))
        $ trace ("p: " <> prettyString p)
          -- We don't really care about the predicate p right now, as
          -- we just want to express the value binding as an index function.
          -- So we skip p and go directly to building one based on the body.
          --
          -- M.singleton vn (E.baseTag vn)
          backwards M.empty body
    _ -> M.empty
  where
    getRes :: E.Exp -> [E.Exp]
    getRes (E.AppExp (E.LetPat _ _p _e body' _) _) =
      getRes body'
    getRes (E.TupLit es _) =
      concatMap getRes es
    getRes e = [e]

-- | Analyse an expression backwards.
backwards :: M.Map E.VName IndexFn -> E.Exp -> M.Map E.VName IndexFn
backwards dict (E.AppExp (E.LetPat _ p e body _) _)
  | (E.Named x, _, _) <- E.patternParam $ fixTuple p =
    trace ("backwards: " <> prettyString p) $
    trace ("backwards: " <> prettyString e) $
    backwards (M.insert x (E.baseTag x) dict) body
  where
    -- fix, only supports wildcards for now
    isWildcard (E.Wildcard {}) = True
    isWildcard _ = False
    fixTuple (E.TuplePat ps _) = head $ filter (not . isWildcard) ps
    fixTuple p = p
backwards dict _ = dict
