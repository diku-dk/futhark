module Futhark.Pass.Flatten.Distribute
  ( distributeMap,
    distributeBody,
    ResMap,
    Distributed (..),
    DistStm (..),
    DistInput (..),
    DistInputs,
    DistType (..),
    distInputType,
    DistResult (..),
    ResTag (..),
  )
where

import Data.Bifunctor (second)
import Data.List qualified as L
import Data.Map qualified as M
import Data.Maybe (fromMaybe, mapMaybe)
import Futhark.IR.SOACS
import Futhark.Util (nubOrd)
import Futhark.Util.Pretty

newtype ResTag = ResTag Int
  deriving (Eq, Ord, Show)

-- | Something that is mapped.
data DistInput
  = -- | A value bound outside the original map nest.  By necessity
    -- regular.  The type is the parameter type.
    DistInputFree VName Type
  | -- | A value constructed inside the original map nest.  May be
    -- irregular.
    DistInput ResTag Type
  deriving (Eq, Ord, Show)

type DistInputs = [(VName, DistInput)]

-- | The type of a 'DistInput'.  This corresponds to the parameter
-- type of the original map nest.
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
  { distStmInputs :: DistInputs,
    distStmResult :: [DistResult],
    distStm :: Stm SOACS
  }
  deriving (Eq, Ord, Show)

-- | First element of tuple are certificates for this result.
--
-- Second is the name to which is should be bound.
--
-- Third is the element type (i.e. excluding segments).
type ResMap = M.Map ResTag ([DistInput], VName, Type)

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

resultMap :: [(VName, DistInput)] -> [DistStm] -> Pat Type -> Result -> ResMap
resultMap avail_inputs stms pat res = mconcat $ map f stms
  where
    f stm =
      foldMap g $ zip (distStmResult stm) (patElems (stmPat (distStm stm)))
    g (DistResult rt _, pe) =
      maybe mempty (M.singleton rt) $ findRes pe
    findRes (PatElem v v_t) = do
      (SubExpRes cs _, pv) <-
        L.find ((Var v ==) . resSubExp . fst) $ zip res $ patNames pat
      Just (map findCert $ unCerts cs, pv, v_t)
    findCert v = fromMaybe (DistInputFree v (Prim Unit)) $ lookup v avail_inputs

splitIrregDims :: Names -> Type -> (Rank, Type)
splitIrregDims bound_outside (Array pt shape u) =
  let (irreg, reg) = second reverse $ span regDim $ reverse $ shapeDims shape
   in (Rank $ length irreg, Array pt (Shape reg) u)
  where
    regDim (Var v) = v `notNameIn` bound_outside
    regDim Constant {} = True
splitIrregDims _ t = (mempty, t)

freeInput :: [(VName, DistInput)] -> VName -> Maybe (VName, DistInput)
freeInput avail_inputs v =
  (v,) <$> lookup v avail_inputs

patInput :: ResTag -> PatElem Type -> (VName, DistInput)
patInput tag pe =
  (patElemName pe, DistInput tag $ patElemType pe)

-- TODO: Split this and 'distributeMap' up in a meaningful way
distributeBody ::
  Scope rep ->
  SubExp ->
  [(VName, DistInput)] ->
  Body SOACS ->
  Distributed
distributeBody outer_scope w param_inputs body =
  let ((_, avail_inputs), stms) =
        L.mapAccumL distributeStm (ResTag (length param_inputs), param_inputs) $
          stmsToList $
            bodyStms body
   in Distributed stms mempty
  where
    bound_outside = namesFromList $ M.keys outer_scope
    paramInput p arr = (paramName p, DistInputFree arr $ paramType p)
    distType t = uncurry (DistType w) $ splitIrregDims bound_outside t
    distributeStm (ResTag tag, avail_inputs) stm =
      let pat = stmPat stm
          new_tags = map ResTag $ take (patSize pat) [tag ..]
          avail_inputs' =
            avail_inputs <> zipWith patInput new_tags (patElems pat)
          free_in_stm = freeIn stm
          used_free = mapMaybe (freeInput avail_inputs) $ namesToList free_in_stm
          used_free_types =
            mapMaybe (freeInput avail_inputs)
              . namesToList
              . foldMap (freeIn . distInputType . snd)
              $ used_free
          stm' =
            DistStm
              (nubOrd $ used_free_types <> used_free)
              (zipWith DistResult new_tags $ map distType $ patTypes pat)
              stm
       in ((ResTag $ tag + length new_tags, avail_inputs'), stm')

distributeMap :: Scope rep -> Pat Type -> SubExp -> [VName] -> Lambda SOACS -> Distributed
distributeMap outer_scope map_pat w arrs lam =
  let param_inputs =
        zipWith paramInput (lambdaParams lam) arrs
      ((_, avail_inputs), stms) =
        L.mapAccumL distributeStm (ResTag 0, param_inputs) $
          stmsToList $
            bodyStms $
              lambdaBody lam
   in Distributed stms $ resultMap avail_inputs stms map_pat $ bodyResult $ lambdaBody lam
  where
    bound_outside = namesFromList $ M.keys outer_scope
    paramInput p arr = (paramName p, DistInputFree arr $ paramType p)
    distType t = uncurry (DistType w) $ splitIrregDims bound_outside t
    distributeStm (ResTag tag, avail_inputs) stm =
      let pat = stmPat stm
          new_tags = map ResTag $ take (patSize pat) [tag ..]
          avail_inputs' =
            avail_inputs <> zipWith patInput new_tags (patElems pat)
          free_in_stm = freeIn stm
          used_free = mapMaybe (freeInput avail_inputs) $ namesToList free_in_stm
          used_free_types =
            mapMaybe (freeInput avail_inputs)
              . namesToList
              . foldMap (freeIn . distInputType . snd)
              $ used_free
          stm' =
            DistStm
              (nubOrd $ used_free_types <> used_free)
              (zipWith DistResult new_tags $ map distType $ patTypes pat)
              stm
       in ((ResTag $ tag + length new_tags, avail_inputs'), stm')
