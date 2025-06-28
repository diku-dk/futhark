module Futhark.Optimise.TensorCores.Utils
  ( gemmName,
    copyGlobalSharedName,
    copyRegistersSharedName,
    isTCName,
    isPrefixOfName,
    getTCName,
    MMMSignature (..),
    mkInt64Const,
    mapStmsWithScope,
  )
where

import Data.List (find, isPrefixOf)
import Futhark.IR

data MMMSignature
  = GemmSignature
      { elmTypeAGemm :: PrimType,
        elmTypeBGemm :: PrimType,
        elmTypeCGemm :: PrimType,
        sizeMGemm :: Int,
        sizeNGemm :: Int,
        sizeKGemm :: Int,
        sizeRegsGemm :: Int
      }
  | CopyGlobalSharedSignature
      { elmTypeCPGS :: PrimType,
        sizeYCPGS :: Int,
        sizeXCPGS :: Int
      }
  | CopyRegistersSharedSignature
      { elmTypeCPRS :: PrimType,
        sizeMCPRS :: Int,
        sizeNCPRS :: Int,
        sizeRegsCPRS :: Int,
        blockSizeCPRS :: Int
      }
  deriving (Show, Eq, Ord)

gemmName :: Name
gemmName = "tensorMMM"

copyGlobalSharedName :: Name
copyGlobalSharedName = "copyGlobalShared"

copyRegistersSharedName :: Name
copyRegistersSharedName = "copyRegistersShared"

isPrefixOfName :: Name -> Name -> Bool
isPrefixOfName prefix name = nameToString prefix `isPrefixOf` nameToString name

funNames :: [Name]
funNames = [gemmName, copyGlobalSharedName, copyRegistersSharedName]

isTCName :: Name -> Bool
isTCName name = any (`isPrefixOfName` name) funNames

getTCName :: Name -> Maybe Name
getTCName name = find (`isPrefixOfName` name) funNames

-- Helper functions

-- | Creates an i64 SubExp
mkInt64Const :: Int -> SubExp
mkInt64Const = Constant . IntValue . intValue Int64

-- | Map a function over stmts and update the scope for each stmt.
mapStmsWithScope ::
  (Monoid a, LocalScope rep f) =>
  (Stm rep -> f a) ->
  Stms rep ->
  f a
mapStmsWithScope f stms =
  case stmsHead stms of
    Nothing -> pure mempty
    Just (stm, stms') -> do
      stm' <- f stm
      stms'' <- inScopeOf stm $ mapStmsWithScope f stms'
      pure $ stm' <> stms''
