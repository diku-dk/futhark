module Futhark.Analysis.View (mkViewProg) where

import Data.List qualified as L
import Data.List.NonEmpty()
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe, fromMaybe)
import Futhark.Analysis.View.Representation
import Futhark.MonadFreshNames
import Futhark.Util.Pretty
import Futhark.SoP.SoP qualified as SoP
import Language.Futhark.Semantic
import Language.Futhark (VName)
import Language.Futhark qualified as E
import qualified Data.Map as M
import Debug.Trace (traceM)
import qualified Data.Set as S
import Futhark.Analysis.View.Rules


--------------------------------------------------------------
tracePrettyM :: (Applicative f, Pretty a) => a -> f ()
tracePrettyM = traceM . prettyString
--------------------------------------------------------------

-- mkViewProg :: VNameSource -> [E.Dec] -> Views
-- mkViewProg vns prog = tracePretty $ execViewM (mkViewDecs prog) vns
mkViewProg :: VNameSource -> Imports -> Views
mkViewProg vns prog = execViewM (mkViewImports prog) vns

mkViewImports :: [(ImportName, FileModule)] -> ViewM ()
mkViewImports = mapM_ (mkViewDecs . E.progDecs . fileProg . snd)
-- A program is a list of declarations (DecBase); functions are value bindings
-- (ValBind). Everything is in an AppExp.

mkViewDecs :: [E.Dec] -> ViewM ()
mkViewDecs [] = pure ()
mkViewDecs (E.ValDec vb : rest) = do
  mkViewValBind vb
  mkViewDecs rest
mkViewDecs (_ : ds) = mkViewDecs ds

mkViewValBind :: E.ValBind -> ViewM ()
mkViewValBind (E.ValBind _ vn ret _ _ params body _ _ _) =
  -- mapM_ paramRefs params
  -- forwards body
  case ret of
    Just (E.TERefine _t _goal _) -> do
      -- We don't really care about the goal right now, as
      -- we just want to express the value binding as an index function.
      traceM ("\n====\nmkViewValBind: " <> prettyString vn)
      traceM ("\nTo prove:\n--------\n" <> prettyString ret)
      traceM ("\nWith params\n-----------\n" <> prettyString params)
      traceM ("\nFor body\n--------\n" <> prettyString body <> "\n====\n")
      forwards body
      pure ()
    _ -> pure ()


getFun :: E.Exp -> Maybe String
getFun (E.Var (E.QualName [] vn) _ _) = Just $ E.baseString vn
getFun _ = Nothing

getVarVName :: E.Exp -> Maybe VName
getVarVName (E.Var (E.QualName [] vn) _ _) = Just vn
getVarVName _ = Nothing

getSize :: E.Exp -> Exp
getSize (E.Var _ (E.Info {E.unInfo = E.Array _ _ shape _}) _)
  | dim:_ <- E.shapeDims shape,
    Just sz <- toExp dim =
  sz
getSize (E.ArrayLit [] (E.Info {E.unInfo = E.Array _ _ shape _}) _)
  | dim:_ <- E.shapeDims shape,
    Just sz <- toExp dim =
  sz
getSize _ = error "donk"

stripExp :: E.Exp -> E.Exp
stripExp x = fromMaybe x (E.stripExp x)

forwards :: E.Exp -> ViewM ()
forwards (E.AppExp (E.LetPat _ p e body _) _)
  | (E.Named x, _, _) <- E.patternParam p = do
    traceM (prettyString p <> " = " <> prettyString e)
    newView <- forward e
    traceM . show $ newView
    tracePrettyM newView
    traceM "🎭 hoisting cases"
    newView1 <- hoistCases newView >>= simplifyPredicates
    tracePrettyM newView1
    newView2 <- substituteViews newView1
    tracePrettyM newView2
    traceM "🎭 hoisting cases"
    newView3 <- hoistCases newView2 >>= simplifyPredicates
    tracePrettyM newView3
    newView4 <- simplifyPredicates (simplify newView3)
    tracePrettyM newView4
    newView5 <- rewrite newView4 >>= simplifyPredicates
    tracePrettyM newView5
    traceM "\n"
    insertView x newView5
    forwards body
    pure ()
forwards _ = pure ()


-- Apply
--   (ExpBase f vn)
--   (NE.NonEmpty (f (Diet, Maybe VName), ExpBase f vn))
--   SrcLoc
--
--  Above, f is Info which adds type info to stuff.
--  First parameter just contains the name map from the prelude here.
--  Second parameter is the map arguments; a non-empty list of pairs
--    (Info (Diet, Maybe VName),
--     ExpBase Info VName)
--  the first of which will be the map lambda (or function)
--  and the rest are the arrays being mapped over
forward :: E.Exp -> ViewM View
forward (E.AppExp (E.Apply f args _) _)
  | Just fname <- getFun f,
    "map" `L.isPrefixOf` fname,
    E.Lambda params body _ _ _ : args' <- getArgs args,
    Just body' <- toExp body = do
      i <- newNameFromString "i"
      let sz = getSize (head args')
      -- Make susbtitutions from function arguments to array names.
      let arrs = mapMaybe getVarVName args'
      let params' = map E.patNames params
      -- TODO params' is a [Set], I assume because we might have
      --   map (\(x, y) -> ...) xys
      -- meaning x needs to be substituted by x[i].0
      let params'' = mconcat $ map S.toList params' -- XXX wrong, see above
      let subst = M.fromList (zip params'' (map (flip Idx (Var i) . Var) arrs))
      substituteName subst $ View (Forall i (Iota sz)) body'
  | Just fname <- getFun f,
    "scan" `L.isPrefixOf` fname, -- XXX support only builtin ops for now
    [E.OpSection (E.QualName [] vn) _ _, _ne, xs'] <- getArgs args,
    Just xs <- toExp xs' = do
      let sz = getSize xs'
      i <- newNameFromString "i"
      op <-
        case E.baseString vn of
          "+" -> pure (~+~)
          "-" -> pure (~-~)
          "*" -> pure (~*~)
          _ -> error ("toExp not implemented for bin op: " <> show vn)
      let e = Cases . NE.fromList $ [(Var i :== SoP (SoP.int2SoP 0),
                                      Idx xs (Var i)),
                                     (Not $ Var i :== SoP (SoP.int2SoP 0),
                                      Recurrence `op` Idx xs (Var i))]
      pure $ View (Forall i (Iota sz)) e
forward e -- No iteration going on here, e.g., `x = if c then 0 else 1`.
  | Just e' <- toExp e = do
    pure $ View Empty e'
forward e = do
    error ("Unhandled exp: " <> prettyString e <> "\n" <> show e)

-- Strip unused information.
getArgs :: NE.NonEmpty (a, E.Exp) -> [E.Exp]
getArgs = map (stripExp . snd) . NE.toList

toExp :: E.Exp -> Maybe Exp
toExp (E.Var (E.QualName _ x) _ _) =
  pure $ Var x
toExp (E.ArrayLit es _ _) =
  let es' = map toExp es
  in  Array <$> sequence es'
toExp (E.AppExp (E.If c t f _) _) = do
  c' <- toExp c
  t' <- toExp t
  f' <- toExp f
  pure $ Cases (NE.fromList [(c', t'), (Not c', f')])
toExp (E.AppExp (E.BinOp (op, _) _ (e_x, _) (e_y, _) _) _)
  | E.baseTag (E.qualLeaf op) <= E.maxIntrinsicTag,
    name <- E.baseString $ E.qualLeaf op,
    Just bop <- L.find ((name ==) . prettyString) [minBound .. maxBound :: E.BinOp] = do
      x <- toExp e_x
      y <- toExp e_y
      case bop of
        E.Plus -> pure $ x ~+~ y
        E.Times -> pure $ x ~*~ y
        E.Minus -> pure $ x ~-~ y
        E.Equal -> pure $ x :== y
        E.Less -> pure $ x :< y
        E.Greater -> pure $ x :> y
        E.LogAnd -> pure $ x :&& y
        _ -> error ("toExp not implemented for bin op: " <> show bop)
toExp (E.AppExp (E.Index xs slice _) _)
  | [E.DimFix i] <- slice = -- XXX support only simple indexing for now
  let i' = toExp i
      xs' = toExp xs
  in  Idx <$> xs' <*> i'
toExp (E.AppExp (E.Apply f args _) _)
  | Just fname <- getFun f,
    fname == "not",
    [arg] <- getArgs args =
  Not <$> toExp arg
toExp (E.Parens e _) = toExp e
toExp (E.Attr _ e _) = toExp e
toExp (E.IntLit x _ _) = pure $ SoP $ SoP.int2SoP x
toExp (E.Negate (E.IntLit x _ _) _) = pure $ SoP $ SoP.negSoP $ SoP.int2SoP x
toExp (E.Literal (E.BoolValue x) _) = pure $ Bool x
toExp e = error ("toExp not implemented for: " <> show e)
