module Futhark.CodeGen.ImpGen.Multicore.Base
 ( toParam
 , compileKBody
 , MulticoreGen
 )
 where

import Data.List
import Prelude hiding (quot, rem)
import qualified Futhark.CodeGen.ImpCode.Multicore as Imp

import Futhark.CodeGen.ImpGen
import Futhark.Representation.ExplicitMemory

type MulticoreGen = ImpM ExplicitMemory Imp.Multicore


toParam :: VName -> TypeBase shape u -> Imp.Param
toParam name t = case t of
                    Prim pt      -> Imp.ScalarParam name pt
                    Mem space'   -> Imp.MemParam name space'
                    Array pt _ _ -> Imp.ScalarParam name pt -- TODO: Fix this!


compileKBody :: (KernelBody ExplicitMemory)
             -> ([(SubExp, [Imp.Exp])] -> ImpM ExplicitMemory Imp.Multicore ())
             -> ImpM ExplicitMemory Imp.Multicore ()
compileKBody kbody red_cont =
  compileStms mempty (kernelBodyStms kbody) $ do
    let red_res = kernelBodyResult kbody
    red_cont $ zip (map kernelResultSubExp red_res) $ repeat []
