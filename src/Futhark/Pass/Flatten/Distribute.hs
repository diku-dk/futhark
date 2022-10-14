module Futhark.Pass.Flatten.Distribute
  ( distributeMap,
    ResMap,
    Distributed (..),
    DistStm (..),
    DistInput (..),
    DistType (..),
    DistResult (..),
    ResTag,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (second)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (mapMaybe)
import Futhark.IR.GPU
import Futhark.IR.SOACS
import Futhark.MonadFreshNames
import Futhark.Util.Pretty

newtype ResTag = ResTag Int
  deriving (Eq, Ord, Show)

-- | Something that is mapped.
data DistInput = DistInputFree VName Type | DistInput ResTag Type
  deriving (Eq, Ord, Show)

distInputType :: DistInput -> Type
distInputType (DistInputFree _ t) = t
distInputType (DistInput _ t) = t

data DistType
  = DistType
      SubExp
      -- ^ Outer regular size.
      Rank
      -- ^ Irregular dimensions on top (but after the leading regular
      -- size).
      Type
      -- ^ The regular "element type" - in the worst case, at least a
      -- scalar.
  deriving (Eq, Ord, Show)

data DistResult = DistResult {distResTag :: ResTag, distResType :: DistType}
  deriving (Eq, Ord, Show)

data DistStm = DistStm
  { distStmInputs :: [(VName, DistInput)],
    distStmResult :: [DistResult],
    distStm :: Stm SOACS
  }
  deriving (Eq, Ord, Show)

type ResMap = M.Map ResTag VName

data Distributed = Distributed [DistStm] ResMap
  deriving (Eq, Ord, Show)

instance Pretty ResTag where
  pretty (ResTag x) = "r" <> pretty x

instance Pretty DistInput where
  pretty (DistInputFree v _) = pretty v
  pretty (DistInput rt _) = pretty rt

instance Pretty DistType where
  pretty (DistType w r t) =
    brackets (pretty w) <> pretty r <> pretty t

instance Pretty DistResult where
  pretty (DistResult rt t) =
    pretty rt <> colon <+> pretty t

instance Pretty DistStm where
  pretty (DistStm inputs res stm) =
    "let" <+> ppTuple' (map pretty res) <+> "=" </> indent 2 stm'
    where
      res' = "return" <+> ppTuple' (map pretty res)
      stm' =
        "map"
          <+> nestedBlock
            "{"
            "}"
            (stack (map onInput inputs ++ [pretty stm, res']))
      onInput (v, inp) =
        "for"
          <+> parens (pretty v <> colon <+> pretty (distInputType inp))
          <+> "<-"
          <+> pretty inp

instance Pretty Distributed where
  pretty (Distributed stms res) =
    stms' </> res'
    where
      res' = stack $ map onRes $ M.toList res
      stms' = stack $ map pretty stms
      onRes (rt, v) = "let" <+> pretty v <+> "=" <+> pretty rt

resultMap :: [DistStm] -> Pat Type -> Result -> M.Map ResTag VName
resultMap stms pat res = mconcat $ map f stms
  where
    res_map = zip (map resSubExp res) (patNames pat)
    f stm =
      foldMap g $ zip (distStmResult stm) (patNames (stmPat (distStm stm)))
    g (DistResult rt _, v) =
      maybe mempty (M.singleton rt) $ lookup (Var v) res_map

splitIrregDims :: Names -> Type -> (Rank, Type)
splitIrregDims bound_outside (Array pt shape u) =
  let (irreg, reg) = second reverse $ span regDim $ reverse $ shapeDims shape
   in (Rank $ length irreg, Array pt (Shape reg) u)
  where
    regDim (Var v) = v `notNameIn` bound_outside
    regDim Constant {} = True
splitIrregDims _ t = (mempty, t)

distributeMap :: Scope SOACS -> Pat Type -> SubExp -> [VName] -> Lambda SOACS -> Distributed
distributeMap outer_scope map_pat w arrs lam =
  let param_inputs =
        zipWith paramInput (lambdaParams lam) arrs
      (_, stms) =
        L.mapAccumL distributeStm (ResTag 0, param_inputs) $
          stmsToList $
            bodyStms $
              lambdaBody lam
   in Distributed stms $ resultMap stms map_pat (bodyResult (lambdaBody lam))
  where
    bound_outside = namesFromList $ M.keys outer_scope
    paramInput p arr = (paramName p, DistInputFree arr $ paramType p)
    freeInput avail_inputs v =
      (v,) <$> lookup v avail_inputs
    patInput tag pe =
      (patElemName pe, DistInput tag $ patElemType pe)
    distType t =
      uncurry (DistType w) $ splitIrregDims bound_outside t
    distributeStm (ResTag tag, avail_inputs) stm =
      let pat = stmPat stm
          new_tags = map ResTag $ take (patSize pat) [tag ..]
          avail_inputs' =
            avail_inputs <> zipWith patInput new_tags (patElems pat)
          stm' =
            DistStm
              (mapMaybe (freeInput avail_inputs) $ namesToList $ freeIn stm)
              (zipWith DistResult new_tags $ map distType $ patTypes pat)
              stm
       in ((ResTag $ tag + length new_tags, avail_inputs'), stm')
