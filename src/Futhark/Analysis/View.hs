module Futhark.Analysis.View (mkViewProg) where

import Data.Map.Strict qualified as M
-- import Futhark.Analysis.Refinement.Monad
import Futhark.MonadFreshNames
import Futhark.Util.Pretty
import Language.Futhark qualified as E
import Language.Futhark.Semantic

-- import Debug.Trace (trace, traceM)
import Debug.Trace (trace)

type View = Int

mkViewProg :: VNameSource -> Imports -> M.Map E.VName View
mkViewProg _vnsource = mkViewImports

mkViewImports :: [(ImportName, FileModule)] -> M.Map E.VName View
mkViewImports = mconcat . map (mkViewDecs . E.progDecs . fileProg . snd)
-- A program is a list of declarations (DecBase); functions are value bindings
-- (ValBind).

mkViewDecs :: [E.Dec] -> M.Map E.VName View
mkViewDecs [] = M.empty
mkViewDecs (E.ValDec vb : rest) =
  mkViewValBind vb `M.union` mkViewDecs rest
mkViewDecs (_ : ds) = mkViewDecs ds

mkViewValBind :: E.ValBind -> M.Map E.VName View
mkViewValBind (E.ValBind _ vn ret _ _ _params body _ _ _) =
  -- mapM_ paramRefs params
  -- forwards body
  case ret of
    Just (E.TERefine t p _) -> do
      trace ("\n====\nmkViewValBind: "
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

-- -- | Analyse an expression forwards.
-- forwards :: M.Map E.VName View -> E.Exp -> M.Map E.VName View
-- forwards dict (E.AppExp (E.LetPat _ p e body _) _)

-- | Analyse an expression backwards.
backwards :: M.Map E.VName View -> E.Exp -> M.Map E.VName View
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
