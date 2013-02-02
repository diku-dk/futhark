module L0.TypeChecker (checkProg) where

import Control.Monad.Identity

import L0.AbSyn

checkProg :: Prog Maybe -> Prog Identity
checkProg = undefined
