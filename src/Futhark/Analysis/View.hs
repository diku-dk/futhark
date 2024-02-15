module Futhark.Analysis.View (mkViewProg) where

import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as M
-- import Futhark.Analysis.Refinement.Monad
import Futhark.MonadFreshNames
import Futhark.Util.Pretty
import Futhark.IR.Pretty() -- To import VName Pretty instance.
import Language.Futhark qualified as E
import Language.Futhark.Semantic

-- import Debug.Trace (trace, traceM)
import Debug.Trace (trace)

tracePretty a = trace (prettyString a) a

mkViewProg :: VNameSource -> Imports -> M.Map E.VName View
mkViewProg _vnsource = mkViewImports

mkViewImports :: [(ImportName, FileModule)] -> M.Map E.VName View
mkViewImports = mconcat . map (mkViewDecs . E.progDecs . fileProg . snd)
-- A program is a list of declarations (DecBase); functions are value bindings
-- (ValBind). Everything is in an AppExp.

mkViewDecs :: [E.Dec] -> M.Map E.VName View
mkViewDecs [] = M.empty
mkViewDecs (E.ValDec vb : rest) =
  mkViewValBind vb `M.union` mkViewDecs rest
mkViewDecs (_ : ds) = mkViewDecs ds

mkViewValBind :: E.ValBind -> M.Map E.VName View
mkViewValBind (E.ValBind _ vn ret _ _ params body _ _ _) =
  -- mapM_ paramRefs params
  -- forwards body
  case ret of
    Just (E.TERefine _t _goal _) ->
      -- We don't really care about the goal right now, as
      -- we just want to express the value binding as an index function.
      let res = forwards M.empty body
      in trace ("\n====\nmkViewValBind: " <> prettyString vn)
         $ trace ("\nTo prove:\n--------\n" <> prettyString ret)
         $ trace ("\nWith params\n-----------\n" <> prettyString params)
         $ trace ("\nFor body\n--------\n" <> prettyString body <> "\n====\n")
         $ tracePretty
         res
    _ -> M.empty
  -- where
  --   getRes :: E.Exp -> [E.Exp]
  --   getRes (E.AppExp (E.LetPat _ _p _e body' _) _) =
  --     getRes body'
  --   getRes (E.TupLit es _) =
  --     concatMap getRes es
  --   getRes e = [e]

data Exp =
    Var E.VName
  | Array [Exp]
  | If Exp Exp Exp
  | Range Exp Exp        -- from ... to
  | Sum Exp Exp Exp Exp  -- index lower_bound upper_bound indexed_expression
  | Idx Exp Exp          -- array index
  -- | SoP (SoP Exp)
  deriving (Show, Eq, Ord)

instance Pretty Exp where
  pretty (Var x) = pretty x
  pretty (Array ts) = pretty ts
  pretty (Idx arr i) = parens (pretty arr) <> "[" <> pretty i <> "]"
  pretty (Sum i lb ub e) =
    "Σ_"
      <> pretty i
      <> "="
      <+> pretty lb
      <> "^"
      <+> pretty ub
      <+> parens (pretty e)
  pretty (If c t f) =
    "If"
      <+> parens (pretty c)
      <+> "then"
      <+> parens (pretty t)
      <+> "else"
      <+> parens (pretty f)
  pretty (Range from to) =
    parens (mconcat $ punctuate comma $ map pretty [from, to]) 

-- toExp (E.Var (E.QualName qs x) _ _) =
--   pure $ Just $ Var x

newtype Domain = Iota Exp -- [0, ..., n-1]
            -- | Union ...
  deriving (Show, Eq, Ord)

instance Pretty Domain where
  pretty (Iota e) = "iota" <+> pretty e

data View = Forall
  { iterator :: E.VName,
    domain :: Domain,
    -- shape :: Maybe Shape, -- Might make sense to use this.
    value :: Int
  }
  deriving (Show, Eq)

instance Pretty View where
  pretty (Forall i dom e) =
    "∀" <> pretty i <+> "∈" <+> pretty dom <+> "." <+> pretty e

type ViewEnv = M.Map E.VName View

instance Pretty ViewEnv where
  pretty env =
    stack $ map (\(a, b) -> pretty a <+> ":" <+> pretty b) $ M.toList env

dummyVName name = E.VName name 0

forwards :: ViewEnv -> E.Exp -> ViewEnv
forwards env (E.AppExp (E.LetPat _ p e body _) _)
  | (E.Named x, _, _) <- E.patternParam p =
    let res = trace ("p: " <> prettyString p) $
              trace ("e: " <> prettyString e) $
              forward e
        env' = trace ("env:" <> show (M.mapKeys prettyString env) <> "\n") $
               M.insert x res env
    in forwards env' body
forwards env _ = env

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
forward :: E.Exp -> View
forward (E.AppExp (E.Apply f args _) _)
  | Just fname <- getFun f,
    "map" `L.isPrefixOf` fname =
    let i = dummyVName "i"
    in  trace (show args) $
        Forall i (Iota $ Var i) 0
forward (E.AppExp (E.Apply f args _) _)
  | Just fname <- getFun f,
    "scan" `L.isPrefixOf` fname =
    let i = dummyVName "i"
    in  Forall i (Iota $ Var i) 1
  -- E.Lambda params body _ _ _ : args' <- map ((\x -> fromMaybe x (E.stripExp x)) . snd) $ NE.toList args -> do
forward (E.AppExp (E.If cond e1 e2 _srcLoc) _) =
    let i = dummyVName "i"
    in  Forall i (Iota $ Var i) 2
forward e =
    let i = dummyVName "i"
    in  trace ("Unhandled exp:" <> prettyString e) $
        trace ("Repr:" <> show e) $
        Forall i (Iota $ Var i) 1337

getFun :: E.Exp -> Maybe String
getFun (E.Var (E.QualName [] vn) _ _) = Just $ E.baseString vn
getFun _ = Nothing

-- getArrayVNames :: NonEmpty a -> [E.VName]
-- getArrayVNames (_ :| args) =
--   mapMaybe (getFun . snd) (toList args)


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
