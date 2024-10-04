module Futhark.Pass.InitNames
    (initNamesPass)
where

import Futhark.IR
import Futhark.Pass
import Futhark.MonadFreshNames
import Futhark.Analysis.SymbolTable
import Futhark.Analysis.UsageTable
import Futhark.Analysis.Alias
import Futhark.IR.TypeCheck
import Futhark.IR.Prop.Aliases
import Futhark.IR.Aliases
import Futhark.Optimise.Simplify.Engine
import Debug.Trace
import Futhark.IR.Parse
import Language.Futhark.Primitive.Parse
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L
import Data.Text qualified as T


initNamesPass :: RepTypes rep => Pass rep rep
initNamesPass = Pass "init-names" "Initialise name source to avoid name clashes" initNames


-- TODO: use scope, maybe symboltable
initNames :: RepTypes rep => Prog rep -> PassM (Prog rep)
initNames p = do
    let maxName = getMaxName p
    putNameSource (newNameSource maxName)
    pure p


-- TODO: should ideally read through program to find next available name
-- TODO: use traverlsals.hs
getMaxName :: Prog rep -> Int
getMaxName = const 12345678


--getMaxName :: RepTypes rep => Prog rep -> Int
--getMaxName p =
--    let programString = show p in
--    --  TODO: just find _\d+ in the string?
--    let test = parse pVName "" (T.pack programString) in
--    trace (show test) 12345678