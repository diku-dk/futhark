module Futhark.Optimise.IntraMMM.Utils where

import Futhark.IR
import Data.List (isPrefixOf)


-- TODO: encode sig in names
data MMMSignature =
    GemmSignature {
      elmTypeAGemm :: PrimType,
      elmTypeBGemm :: PrimType,
      elmTypeCGemm :: PrimType,
      sizeMGemm :: Int,
      sizeNGemm :: Int,
      sizeKGemm :: Int,
      sizeRegsGemm :: Int
    }
  | CopyGlobalSharedSignature {
      elmTypeCPGS :: PrimType,
      sizeYCPGS :: Int,
      sizeXCPGS :: Int
    }
  | CopyRegistersSharedSignature {
      elmTypeCPRS :: PrimType,
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
isPrefixOfName prefix name = show prefix `isPrefixOf` show name

funNames :: [Name]
funNames = [gemmName, copyGlobalSharedName, copyRegistersSharedName]

isMMMName :: Name -> Bool
isMMMName name = any (`isPrefixOfName` name) funNames
