module Futhark.Optimise.IntraMMM.Utils where

import Futhark.IR

gemmName :: Name
gemmName = "gemm_123456"

copyGlobalSharedName :: Name
copyGlobalSharedName = "copyGlobalShared"

copyRegistersSharedName :: Name
copyRegistersSharedName = "copyRegistersShared"

funNames :: [Name]
funNames = [gemmName, copyGlobalSharedName, copyRegistersSharedName]