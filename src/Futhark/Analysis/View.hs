module Futhark.Analysis.View (mkViewProg) where

import Data.List qualified as L
import Data.List.NonEmpty()
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe, fromMaybe)
import Futhark.Analysis.View.Representation
-- import Futhark.Analysis.Refinement.Monad
-- import Futhark.Analysis.Refinement.CNF
import Futhark.MonadFreshNames
import Futhark.Util.Pretty
-- import Futhark.IR.Pretty() -- To import VName Pretty instance.
import Futhark.SoP.SoP qualified as SoP
import Language.Futhark.Semantic
import Language.Futhark (VName)
import Language.Futhark qualified as E
import qualified Data.Map as M


--------------------------------------------------------------
import Debug.Trace (trace, traceM)
tracePretty :: Pretty a => a -> a
tracePretty a = trace (prettyString a <> "\n") a

tracePrettyM :: (Applicative f, Pretty a) => a -> f ()
tracePrettyM a = traceM (prettyString a <> "\n")
--------------------------------------------------------------

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
  -- where
  --   getRes :: E.Exp -> [E.Exp]
  --   getRes (E.AppExp (E.LetPat _ _p _e body' _) _) =
  --     getRes body'
  --   getRes (E.TupLit es _) =
  --     concatMap getRes es
  --   getRes e = [e]


getFun :: E.Exp -> Maybe String
getFun (E.Var (E.QualName [] vn) _ _) = Just $ E.baseString vn
getFun _ = Nothing

getVarVName :: E.Exp -> Maybe VName
getVarVName (E.Var (E.QualName [] vn) _ _) = Just vn
getVarVName _ = Nothing

stripExp :: E.Exp -> E.Exp
stripExp x = fromMaybe x (E.stripExp x)


forwards :: E.Exp -> ViewM ()
forwards (E.AppExp (E.LetPat _ p e body _) _)
  | (E.Named x, _, _) <- E.patternParam p = do
    traceM (prettyString p <> " = " <> prettyString e)
    res <- forward e
    tracePrettyM res
    insertView x res
    forwards body
    pure ()
forwards _ = pure ()

-- TODO Build view for `map`.
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
-- TODO 1. Convert lambda in maps to expression representation here.
--         - Add SoP to Exp
--         - Define to toExp
-- TODO 2. Substitute bound args "c" for "Idx conds i" etc.
--         - See Substitute and astMappable and astMap in Robert's code
-- TODO I think this work can be rebased on top of master?
--      Just don't include the part that actually refines types or annotates
--      types in the source.
-- TODO use VNameSource for fresh names
-- TODO make this monadic once we have a MWE
forward :: E.Exp -> ViewM View
forward (E.AppExp (E.Apply f args _) _)
  | Just fname <- getFun f,
    "map" `L.isPrefixOf` fname,
    E.Lambda params body _ _ _ : args' <- map (stripExp . snd) $ NE.toList args,
    Just e <- toExp body = do
      i <- newNameFromString "i"
      let arrs = mapMaybe getVarVName args'
      let params' = map E.patNames params
      traceM (show arrs)
      traceM (show params')
      traceM (show e)
      -- let subst = M.fromList (zip params' arrs)
      -- e' <- SoP.substituteOne (head params', head arrs) <$> e
      -- XXX substitute `Idx conds i` for `c` in `e` using something
      -- like transformNames below (copied from src/Futhark/Internalise/Defunctorise.hs).
      pure $ Forall i (Iota $ Var i) e
forward (E.AppExp (E.Apply f args _) _)
  | Just fname <- getFun f,
    "scan" `L.isPrefixOf` fname = do
      i <- newNameFromString "i"
      pure $ Forall i (Iota $ Var i) (Var i)
forward (E.AppExp (E.If cond e1 e2 _srcLoc) _) = do
      i <- newNameFromString "i"
      pure $ Forall i (Iota $ Var i) (Var i)
forward e = do
    traceM ("Unhandled exp:" <> prettyString e)
    error (show e)

toExp :: E.Exp -> Maybe Exp
toExp (E.Var (E.QualName _ x) _ _) =
  Just $ Var x
toExp (E.ArrayLit es _ _) =
  let es' = map toExp es
  in  Array <$> sequence es'
toExp (E.AppExp (E.If c t f _) _) =
  let c' = toExp c -- TODO Convert to CNF.
      t' = toExp t
      f' = toExp f
  in  If <$> c' <*> t' <*> f'
-- toExp (E.AppExp (E.Apply f args _) _) =
toExp (E.Parens e _) = toExp e
toExp (E.Attr _ e _) = toExp e
toExp (E.IntLit x _ _) = Just $ SoP $ SoP.int2SoP x
toExp e = error ("toExp not implemented for: " <> show e)

-- -- | A general-purpose substitution of names.
-- transformNames :: ASTMappable x => x -> ViewM x
-- transformNames x = do
--   scope <- gets views
--   pure $ runIdentity $ astMap (substituter scope) x
--   where
--     substituter scope =
--       ASTMapper
--         { mapOnExp = onExp scope,
--           mapOnName = \v ->
--             pure $ qualLeaf $ fst $ lookupSubstInScope (qualName v) scope,
--           mapOnStructType = astMap (substituter scope),
--           mapOnPatType = astMap (substituter scope),
--           mapOnStructRetType = astMap (substituter scope),
--           mapOnPatRetType = astMap (substituter scope)
--         }
--     onExp scope e =
--       -- One expression is tricky, because it interacts with scoping rules.
--       case e of
--         QualParens (mn, _) e' _ ->
--           case lookupMod' mn scope of
--             Left err -> error err
--             Right mod ->
--               astMap (substituter $ modScope mod <> scope) e'
--         _ -> astMap (substituter scope) e

-- args:

-- (Info {unInfo = (Observe,Nothing)},
--  Parens (
--    Lambda
--    [Id (VName (Name "c") 6086) (Info {unInfo = Scalar (Prim Bool)}) noLoc]
--    (AppExp (Apply (Var (QualName {qualQuals = [VName (Name "i64") 2794], qualLeaf = VName (Name "bool") 2752}) (Info {unInfo = Scalar (Arrow (fromList []) Unnamed Observe (Scalar (Prim Bool)) (RetType {retDims = [], retType = Scalar (Prim (Signed Int64))}))}) noLoc) ((Info {unInfo = (Observe,Nothing)},Var (QualName {qualQuals = [], qualLeaf = VName (Name "c") 6086}) (Info {unInfo = Scalar (Prim Bool)}) noLoc) :| []) noLoc) (Info {unInfo = AppRes {appResType = Scalar (Prim (Signed Int64)), appResExt = []}}))
--   Nothing
--   (Info {unInfo = (fromList [AliasBound {aliasVar = VName (Name "bool") 2752}],RetType {retDims = [], retType = Scalar (Prim (Signed Int64))})})
--   noLoc
--  )
--  noLoc
-- )

-- :|
-- [(
--   Info {unInfo = (Observe,Nothing)},
--   Var
--     (QualName {qualQuals = [], qualLeaf = VName (Name "conds") 6070})
--     (Info {unInfo = Array (fromList [AliasBound {aliasVar = VName (Name "conds") 6070}]) Nonunique (Shape {shapeDims = [Var (QualName {qualQuals = [], qualLeaf = VName (Name "n") 6068}) (Info {unInfo = Scalar (Prim (Signed Int64))}) noLoc]}) (Prim Bool)})
--     noLoc
-- )]


-- :| is cons for non empty lists,
-- so we have head :| [...]
