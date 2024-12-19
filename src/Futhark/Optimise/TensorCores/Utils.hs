module Futhark.Optimise.TensorCores.Utils
  ( gemmName,
    copyGlobalSharedName,
    copyRegistersSharedName,
    isMMMName,
    isPrefixOfName,
    getMMMName,
    MMMSignature (..),
  )
where

import Data.List (find, isPrefixOf)
import Futhark.IR

-- TODO: encode sig in names
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

isMMMName :: Name -> Bool
isMMMName name = any (`isPrefixOfName` name) funNames

getMMMName :: Name -> Maybe Name
getMMMName name = find (`isPrefixOfName` name) funNames
