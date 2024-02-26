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
import Control.Monad.Identity
import Debug.Trace (trace, traceM)
import qualified Data.Set as S
import Futhark.Analysis.View.Rules


--------------------------------------------------------------
tracePretty :: Pretty a => a -> a
tracePretty a = trace (prettyString a <> "\n") a

tracePrettyM :: (Applicative f, Pretty a) => a -> f ()
tracePrettyM = traceM . prettyString
--------------------------------------------------------------

-- mkViewProg :: VNameSource -> [E.Dec] -> Views
-- mkViewProg vns prog = tracePretty $ execViewM (mkViewDecs prog) vns
mkViewProg :: VNameSource -> Imports -> Views
mkViewProg vns prog = tracePretty $ execViewM (mkViewImports prog) vns

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

-- (QualName {qualQuals = [], qualLeaf = VName (Name "conds") 6070})
-- (Info {unInfo =
--   Array (fromList [AliasBound {aliasVar = VName (Name "conds") 6070}])
--   Nonunique
--   (Shape {shapeDims = [Var (QualName {qualQuals = [], qualLeaf = VName (Name "n") 6068}) (Info {unInfo = Scalar (Prim (Signed Int64))}) noLoc]})
--   (Prim Bool)})
-- noLoc

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

-- ArrayLit [ExpBase f vn] (f PatType) SrcLoc
-- type PatType = TypeBase Size Aliasing

-- -- | An expanded Futhark type is either an array, or something that
-- -- can be an element of an array.  When comparing types for equality,
-- -- function parameter names are ignored.  This representation permits
-- -- some malformed types (arrays of functions), but importantly rules
-- -- out arrays-of-arrays.
-- data TypeBase dim as
--   = Scalar (ScalarTypeBase dim as)
--   | Array as Uniqueness (Shape dim) (ScalarTypeBase dim ())
--   deriving (Eq, Ord, Show)

stripExp :: E.Exp -> E.Exp
stripExp x = fromMaybe x (E.stripExp x)

toCases :: Exp -> Cases Exp
toCases e = Cases (NE.singleton (Bool True, e))

forwards :: E.Exp -> ViewM ()
forwards (E.AppExp (E.LetPat _ p e body _) _)
  | (E.Named x, _, _) <- E.patternParam p = do
    traceM (prettyString p <> " = " <> prettyString e)
    newView <- forward e
    tracePrettyM newView
    newView' <- substituteViews newView
    tracePrettyM newView'
    traceM "\n"
    insertView x newView'
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
--
-- TODO I think this work can be rebased on top of master?
--      Just don't include the part that actually refines types or annotates
--      types in the source.
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
      substituteName subst $ View (Forall i (Iota sz)) (toCases body')
  -- | Just fname <- getFun f,
  --   "scan" `L.isPrefixOf` fname, -- XXX support only builtin ops for now
  --   [E.OpSection (E.QualName [] vn) _ _, _ne, xs'] <- getArgs args = do
  --     view <- forward xs'
  --     xs <- newNameFromString "scan_xs"
  --     insertView xs view

  --     i <- newNameFromString "i"
  --     e <-
  --       case E.baseString vn of
  --         "+" -> pure $ Recurrence ~+~ Idx (Var xs) (Var i)
  --         "-" -> pure $ Recurrence ~-~ Idx (Var xs) (Var i)
  --         "*" -> pure $ Recurrence ~*~ Idx (Var xs) (Var i)
  --         _ -> error ("toExp not implemented for bin op: " <> show vn)
  --     pure $ View (Forall i (Iota $ Var i)) (toCases e)
forward e -- No iteration going on here, e.g., `x = if c then 0 else 1`.
  | Just e' <- toExp e = do
    i <- newNameFromString "i"
    pure $ View Empty (toCases e')
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
  pure $ If c' t' f'
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
toExp (E.Parens e _) = toExp e
toExp (E.Attr _ e _) = toExp e
toExp (E.IntLit x _ _) = pure $ SoP $ SoP.int2SoP x
toExp (E.Negate (E.IntLit x _ _) _) = pure $ SoP $ SoP.negSoP $ SoP.int2SoP x
toExp (E.Literal (E.BoolValue x) _) = pure $ Bool x
toExp e = error ("toExp not implemented for: " <> show e)

substituteName :: ASTMappable x => M.Map VName Exp -> x -> ViewM x
substituteName substitutions x = do
  pure $ runIdentity $ astMap (substituter substitutions) x
  where
    substituter subst =
      ASTMapper
        { mapOnExp = onExp subst }
    onExp subst e@(Var x') =
      case M.lookup x' subst of
        Just x'' -> pure x''
        Nothing -> pure e
    onExp subst e = astMap (substituter subst) e
