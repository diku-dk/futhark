{-# LANGUAGE LambdaCase #-}

-- | Helper for the Efficient sequentialization pass aimed at 
--     optimising intra-group kernels
--   Extends the MSc thesis work of Christian Marslev & Jonas Grønborg
-- 

module Futhark.Optimise.EffSeqIntra.Helper
  ( Env(..), 
    setMapping,
    updateMapping,
    memberMapping,
    lookupMapping,
    updateEnvTid,
    getThreadId,
    findSeqAttr,
    getSeqFactor,
    shouldSequentialize
  ) 
where

import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Set qualified as S
-- import Futhark.Builder.Class
-- import Futhark.Construct
import Futhark.IR.GPU
-- import Futhark.IR.GPU.Simplify (simplifyGPU)
-- import Futhark.Pass
-- import Futhark.Transform.Rename
-- import Futhark.Transform.Substitute


-- | A structure for convenient passing of different information needed at
-- various stages during the pass.
data Env = Env
  { grpId :: SubExp, -- The group id
    grpSize :: SubExp, -- The group size after seq
    grpsizeOld :: SubExp, -- The group size before seq
    threadId :: Maybe VName, -- the thread id if available
    nameMap :: M.Map VName VName, -- Mapping from arrays to tiles
    seqFactor :: SubExp
  }
  deriving (Show)

setMapping :: Env -> M.Map VName VName -> Env
setMapping (Env gid gSize gSizeOld tid _ factor) mapping =
  Env gid gSize gSizeOld tid mapping factor

updateMapping :: Env -> M.Map VName VName -> Env
updateMapping env mapping =
  let mapping' = mapping `M.union` nameMap env
   in setMapping env mapping'

memberMapping :: Env -> VName -> Bool
memberMapping env name = M.member name (nameMap env)

lookupMapping :: Env -> VName -> Maybe VName
lookupMapping env name
  | M.member name (nameMap env) = do
      case M.lookup name (nameMap env) of
        Just n ->
          case lookupMapping env n of
            Nothing -> Just n
            n' -> n'
        Nothing -> Nothing
lookupMapping _ _ = Nothing

updateEnvTid :: Env -> VName -> Env
updateEnvTid (Env gid sz szo _ tm sq) tid = Env gid sz szo (Just tid) tm sq

getThreadId :: Env -> VName
getThreadId env =
  case threadId env of
    (Just tid) -> tid
    _ -> error "No tid to get"

findSeqAttr :: Attrs -> Maybe Attr
findSeqAttr (Attrs attrs) =
  let attrs' = S.toList attrs
   in case L.findIndex isSeqFactor attrs' of
        Just i -> Just $ attrs' !! i
        Nothing -> Nothing
  where
    isSeqFactor :: Attr -> Bool
    isSeqFactor (AttrComp "seq_factor" [AttrInt _]) = True
    isSeqFactor _ = False

getSeqFactor :: Attrs -> SubExp
getSeqFactor attrs =
  case findSeqAttr attrs of
    Just i' ->
      let (AttrComp _ [AttrInt x]) = i'
      in intConst Int64 x
    Nothing -> intConst Int64 4

shouldSequentialize :: Attrs -> Bool
shouldSequentialize attrs =
  case findSeqAttr attrs of
    Just _ -> True
    Nothing -> False

