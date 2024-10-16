module Futhark.Optimise.IntraMMM.Utils where

import Futhark.IR

gemmName :: Name
gemmName = "gemm_123456"

copyGlobalSharedName :: Name
copyGlobalSharedName = "copyGlobalShared"

copyRegistersGlobalName :: Name
copyRegistersGlobalName = "copyRegistersGlobal"

funNames :: [Name]
funNames = [gemmName, copyGlobalSharedName, copyRegistersGlobalName]