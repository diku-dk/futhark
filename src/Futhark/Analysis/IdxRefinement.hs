module Futhark.Analysis.IdxRefinement where

import Control.Monad.RWS
import Data.List qualified as L
import Data.Map qualified as M
import Data.String
import Futhark.Analysis.Refinement.CNF
import Futhark.Analysis.Refinement.Convert
import Futhark.Analysis.Refinement.Forward
import Futhark.Analysis.Refinement.Latex
import Futhark.Analysis.Refinement.Monad
import Futhark.Analysis.Refinement.Prop
import Futhark.Analysis.Refinement.Representation
import Futhark.Analysis.Refinement.Rules
import Futhark.MonadFreshNames
import Futhark.SoP.SoP hiding (Range, SoP, Term)
import Futhark.Util.Pretty
import Language.Futhark qualified as E
import Language.Futhark.Semantic
import Text.LaTeX.Packages.AMSMath qualified as Math

import Debug.Trace (trace, traceM)

refineProg :: VNameSource -> Imports -> [Log]
refineProg vns prog = execRefineM (refineImports prog) vns

refineImports :: [(ImportName, FileModule)] -> RefineM ()
refineImports = mapM_ (refineDecs . E.progDecs . fileProg . snd)

refineDecs :: [E.Dec] -> RefineM ()
refineDecs [] = pure ()
refineDecs (E.ValDec vb : rest) = do
  refineValBind vb
  refineDecs rest
refineDecs (_ : ds) = refineDecs ds

refineValBind :: E.ValBind -> RefineM ()
refineValBind (E.ValBind vn _ ret _ _ params body _ _ _) = do
  mapM paramRefs params
  forwards body
  case ret of
    Just (E.TERefine t p _) -> do
      traceM ("\n====\nrefineValBind\n\nTo prove\n--------\n" <> prettyString ret)
      traceM ("\nFor body\n--------\n" <> prettyString body <> "\n====\n")
      traceM ("getRes: " <> prettyString (getRes body))
      traceM ("p: " <> prettyString p)
      goal <- mkProp p $ getRes body
      traceM ("goal: " <> show goal)
      s <- get
      -- traceM ("s: " <> show (types s))
      res <- backwards (fmap (\g -> (g, s, mempty)) goal) body
      pure ()
    _ -> pure ()
  where
    getRes :: E.Exp -> [E.Exp]
    getRes (E.AppExp (E.LetPat _ p e body _) _) =
      getRes body
    getRes (E.TupLit es _) =
      concatMap getRes es
    getRes e = [e]

    paramRefs (E.PatParens p _) = paramRefs p
    paramRefs (E.PatAttr _ p _) = paramRefs p
    -- QQ: commented out; I think this is for refinement typing on parameters
    --
    -- paramRefs (E.PatAscription e@(E.Id v _ _) (E.TERefine t (E.Lambda [pat] body _ _ _) _) _)
    --   | (E.Named x, _, _) <- E.patternParam pat = do
    --       -- fix, can't handle or
    --       info <-
    --         (concat . cnfToLists . substituteOne (x, Var v))
    --           <$> unsafeConvert toCNF body -- Fix
    --       insertType v (E.patternType e)
    --       modify $ \senv ->
    --         senv
    --           { known = known senv ++ info,
    --             known_map =
    --               M.insertWith (<>) v info $ known_map senv
    --           }
    paramRefs e
      | (E.Named x, _, _) <- E.patternParam e =
          -- trace ("paramRefs " <> prettyString x <> " " <> prettyString(E.patternType e)) $ insertType x (E.patternType e)
          insertType x (E.patternType e)
    paramRefs _ = pure ()

mkProp :: E.Exp -> [E.Exp] -> RefineM (CNF Prop)
mkProp (E.Lambda ps body _ _ _) args = do
  m <- mconcat <$> zipWithM mkSubstParam (concatMap unwrapTuple ps) args
  traceM ("mkProp substitions: " <> show m)
  g <- substitute m <$> unsafeConvert toCNF body
  -- traceM ("mkProp goal: " <> prettyString g)
  -- tell [Math.text "Proving: " <> toLaTeX g]
  pure g
  where
    unwrapTuple (E.TuplePat ps _) = ps
    unwrapTuple (E.RecordPat ps _) = map snd ps
    unwrapTuple p = [p]
    mkSubstParam p arg
      | (E.Named x, _, _) <- E.patternParam p = do
          arg' <- unsafeConvert toExp arg
          pure $ M.singleton x arg'
      | otherwise = do
          error $ unlines [prettyString p, prettyString arg]
          pure mempty
mkProp e _ =
  error $
    "Unsupported predicate: " <> prettyString e

instance Show VNameSource where
  show (VNameSource i) = show i

rewriteProps :: CNF (Prop, SEnv, [Log]) -> RefineM (CNF (Prop, SEnv, [Log]))
rewriteProps gs = do
  gs' <- bindCNFM rewriteProp gs
  let cnf = fmap (\(g, _, _) -> flatten g) gs'
      ws = foldMap (\(_, _, w) -> w) gs'
      gs'' = fmap (\(g, s, _) -> (flatten g, s, [])) gs'
  if not (null ws)
    then do
      tell $ L.nub $ ws
      tell [toLaTeX cnf]
      rewriteProps gs''
    else pure gs''
  where
    rewriteProp :: Prop -> CNFM Prop
    rewriteProp g = do
      g' <- simplify g
      mg' <- applyRules g'
      case mg' of
        Nothing -> pure g'
        Just (s, g'') -> do
          known_props <- gets known
          tell $
            [Math.text "Rewrote " <> toLaTeX g <> Math.text " using " <> Math.mathtt (fromString s)]
          pure g''

unsafeConvert :: (E.Exp -> RefineM (Maybe a)) -> E.Exp -> RefineM a
unsafeConvert f e = do
  me' <- f e
  case me' of
    Nothing -> error $ "Couldn't convert Exp!: " <> prettyString e
    Just e' -> pure e'

backwards :: (CNF (Prop, SEnv, [Log])) -> E.Exp -> RefineM (CNF (Prop, SEnv, [Log]))
backwards gs (E.AppExp (E.LetPat _ p e body _) _)
  | (E.Named x, _, _) <- E.patternParam $ fixTuple p = do
      gs' <- backwards gs body
      insertExp x e
      rewriteProps $ addExps x e $ gs'
  where
    -- fix, only supports wildcards for now
    isWildcard (E.Wildcard {}) = True
    isWildcard _ = False
    fixTuple (E.TuplePat ps _) = head $ filter (not . isWildcard) ps
    fixTuple p = p
    addExps x e =
      fmap
        ( \(g, senv, ws) ->
            (g, senv {exps = M.insert x e $ exps senv}, ws)
        )
backwards gs _ = do
  rewriteProps gs
