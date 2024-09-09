module Futhark.CLI.Fmt.Type (
  fmtTypeBind
  ) where

import Futhark.CLI.Fmt.Format
import Data.Text qualified as T
import Language.Futhark

-- | Boolean indicates whether tuple is single line
fmtTupleTypeElems :: [UncheckedTypeExp] -> Bool -> FormatState Fmt
fmtTupleTypeElems [] _ = pure []
fmtTupleTypeElems [t] _ = fmtTypeExp t
fmtTupleTypeElems (t : ts) isSingle = do
  t' <- fmtTypeExp t
  ts' <- fmtTupleTypeElems ts isSingle
  if isSingle then pure [T.concat $ t' <> [", "] <> ts'] 
  else pure $ t' <> prependComma ts' -- TO DO: Make the comma not be on its own line  
  where prependComma :: Fmt -> Fmt
        prependComma [] = [] -- comma still on seperate linethat's probably pretty slow, better way to do this?
        prependComma fmt = [T.concat $ [", "] <> fmt] 

-- | Formatting of Futhark type expressions.
fmtTypeExp :: UncheckedTypeExp -> FormatState Fmt
fmtTypeExp (TEVar v loc) = do
  c <- comment loc
  pure $ c <> [prettyText v]
fmtTypeExp (TETuple ts loc) | isSingleLine loc = do
  c <- comment loc
  ts' <- fmtTupleTypeElems ts True
  pure $ c <> ["(" <> T.concat ts' <> ")"]
fmtTypeExp (TETuple ts loc) = do
  c <- comment loc
  ts' <- fmtTupleTypeElems ts False
  pure $ c <> ["("] <>  ts' <> [")"]
fmtTypeExp (TEParens type_exp loc) = undefined
fmtTypeExp (TERecord ts loc) = undefined -- Records
fmtTypeExp (TEArray size_exp type_exp loc) = undefined -- A array with an size expression
fmtTypeExp (TEUnique type_exp loc) = undefined -- This "*" https://futhark-lang.org/blog/2022-06-13-uniqueness-types.html
fmtTypeExp (TEApply type_exp type_arg_exp loc) = undefined -- I am not sure I guess applying a higher kinded type to some type expression
fmtTypeExp (TEArrow (Just name) type_exp type_exp' loc) = undefined -- is this "->"?
fmtTypeExp (TEArrow Nothing type_exp type_exp' loc) = undefined -- is this "->"?
fmtTypeExp (TESum type_exps loc) = undefined -- This should be "|"
fmtTypeExp (TEDim names exp loc) = undefined -- This this probably [n][m]expression for array dimensions

fmtTypeParam :: UncheckedTypeParam -> FormatState Fmt
fmtTypeParam (TypeParamDim name loc) = undefined 
fmtTypeParam (TypeParamType l name loc) = undefined

fmtTypeBind :: UncheckedTypeBind -> FormatState Fmt
fmtTypeBind (TypeBind name l ps e NoInfo dc loc) = do
  dc' <- fmtDocComment dc
  ps' <- fmtMany fmtTypeParam ps
  if isSingleLine loc then do
    e' <- fmtTypeExp e
    pure $ dc' <> ["type " <>
                   prettyText l <>
                   prettyText name <>
                   T.intercalate " " ps' <>
                   " = " <>
                   T.concat e']
  else do
    -- incrIndent
    e' <- fmtTypeExp e
    -- decrIndent
    pure $ dc' <> ["type " <>
                  prettyText l <>
                  prettyText name <>
                  T.intercalate " " ps' <>
                  " = "] <> e'
