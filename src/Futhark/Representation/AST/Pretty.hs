{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-- | Futhark prettyprinter.  This module defines 'Pretty' instances
-- for the AST defined in "Futhark.Representation.AST.Syntax",
-- but also a number of convenience functions if you don't want to use
-- the interface from 'Pretty'.
module Futhark.Representation.AST.Pretty
  ( ppType
  , ppValue
  , ppValues
  , ppBinding
  , ppBody
  , ppExp
  , ppSubExp
  , ppLambda
  , ppFun
  , ppTuple
  , prettyPrint
  , PrettyLore (..)
  )
  where

import Data.Array

import Text.PrettyPrint.Mainland

import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Attributes

-- | The class of lores whose annotations can be prettyprinted.
class PrettyLore lore where
  ppBindingLore :: Binding lore -> Maybe Doc
  ppBindingLore = const Nothing

-- | The document @'apply' ds@ separates @ds@ with commas and encloses them with
-- parentheses.
apply :: [Doc] -> Doc
apply = encloseSep lparen rparen comma . map align

commastack :: [Doc] -> Doc
commastack = align . stack . punctuate comma

instance Pretty Uniqueness where
  ppr Unique = star
  ppr Nonunique = empty

instance Pretty Value where
  ppr (BasicVal bv) = ppr bv
  ppr v@(ArrayVal a t)
    | Just s <- arrayString v = text $ show s
    | [] <- elems a = text "empty" <> parens (ppr t)
    | Array {} <- t = brackets $ commastack $ map ppr $ elems a
    | otherwise     = brackets $ commasep $ map ppr $ elems a

instance Pretty (TypeBase Shape) where
  ppr (Basic et) = ppr et
  ppr (Array et (Shape ds) u) = ppr u <> foldr f (ppr et) ds
    where f e s = brackets $ s <> comma <> ppr e

instance Pretty (TypeBase ExtShape) where
  ppr (Basic et) = ppr et
  ppr (Array et (ExtShape ds) u) = ppr u <> foldr f (ppr et) ds
    where f (Free e) s = brackets $ s <> comma <> ppr e
          f (Ext x)  s = brackets $ s <> comma <>
                         text "?" <> text (show x)

instance Pretty (TypeBase Rank) where
  ppr (Basic et) = ppr et
  ppr (Array et (Rank n) u) = u' <> foldl f (ppr et) [1..n]
    where f s _ = brackets s
          u' | Unique <- u = star
             | otherwise = empty

instance Pretty (IdentBase shape) where
  ppr = text . textual . identName

hasArrayLit :: SubExp -> Bool
hasArrayLit (Constant val _) = hasArrayVal val
hasArrayLit _ = False

hasArrayVal :: Value -> Bool
hasArrayVal (ArrayVal {}) = True
hasArrayVal _ = False

instance Pretty SubExp where
  ppr (Var v)        = ppr v
  ppr (Constant v _) = ppr v

instance PrettyLore lore => Pretty (Body lore) where
  ppr (Body lore (bnd:bnds) res) =
    ppr bnd <+> text "in" </> ppr (Body lore bnds res)
  ppr (Body _ [] (Result cs es _))
    | any hasArrayLit es = ppCertificates cs <> braces (commastack $ map ppr es)
    | otherwise          = ppCertificates cs <> braces (commasep   $ map ppr es)

bindingAnnotation :: PrettyLore lore => Binding lore -> Doc -> Doc
bindingAnnotation bnd doc =
  case ppBindingLore bnd of
    Nothing    -> doc
    Just annot -> annot </> doc

instance PrettyLore lore => Pretty (Binding lore) where
  ppr bnd@(Let pat _ e) =
    bindingAnnotation bnd $ align $
    text "let" <+> align (ppPattern $ patternIdents pat) <+>
    (if linebreak
     then equals </> indent 2 (ppr e)
     else equals <+> align (ppr e))
    where linebreak = case e of
                        LoopOp {} -> True
                        If {} -> True
                        PrimOp (SubExp (Constant (ArrayVal {}) _)) -> False
                        PrimOp (ArrayLit {}) -> False
                        _ -> False

instance PrettyLore lore => Pretty (PrimOp lore) where
  ppr (SubExp se) = ppr se
  ppr (ArrayLit [] rt _) =
    text "empty" <> parens (ppr rt)
  ppr (ArrayLit es rt _) =
    case rt of
      Array {} -> brackets $ commastack $ map ppr es
      _        -> brackets $ commasep   $ map ppr es
  ppr (BinOp bop x y _ _) = ppBinOp bop x y
  ppr (Not e _) = text "not" <+> pprPrec 9 e
  ppr (Negate e _) = text "-" <> pprPrec 9 e
  ppr (Index cs v idxs _) =
    ppCertificates cs <> ppr v <>
    brackets (commasep (map ppr idxs))
  ppr (Update cs src idxs ve _) =
    ppCertificates cs <> ppr src <+>
    text "with" <+> brackets (commasep (map ppr idxs)) <+>
    text "<-" <+> align (ppr ve)
  ppr (Iota e _) = text "iota" <> parens (ppr e)
  ppr (Replicate ne ve _) =
    text "replicate" <> apply [ppr ne, align (ppr ve)]
  ppr (Reshape cs shape e _) =
    ppCertificates cs <> text "reshape" <> apply [apply (map ppr shape), ppr e]
  ppr (Rearrange cs perm e _) =
    ppCertificates cs <> text "rearrange" <> apply [apply (map ppr perm), ppr e]
  ppr (Rotate cs n e _) =
    ppCertificates cs <> text "rotate" <> apply [ppr n, ppr e]
  ppr (Split cs e a _ _) =
    ppCertificates cs <> text "split" <> apply [ppr e, ppr a]
  ppr (Concat cs x y _ _) =
    ppCertificates cs <> text "concat" <> apply [ppr x, ppr y]
  ppr (Copy e _) = text "copy" <> parens (ppr e)
  ppr (Assert e _) = text "assert" <> parens (ppr e)
  ppr (Conjoin es _) = text "conjoin" <> parens (commasep $ map ppr es)

instance PrettyLore lore => Pretty (LoopOp lore) where
  ppr (DoLoop respat mergepat i bound loopbody _) =
    text "loop" <+> ppPattern respat <+>
    text "<-" <+> ppPattern pat <+> equals <+> ppTuple' initexp </>
    text "for" <+> ppr i <+> text "<" <+> align (ppr bound) <+> text "do" </>
    indent 2 (ppr loopbody)
    where (pat, initexp) = unzip mergepat
  ppr (Map cs lam as _) =
    ppCertificates' cs <> ppSOAC "mapT" [lam] Nothing as
  ppr (Reduce cs lam inputs _) =
    ppCertificates' cs <> ppSOAC "reduceT" [lam] (Just es) as
    where (es, as) = unzip inputs
  ppr (Redomap cs outer inner es as _) =
    ppCertificates' cs <> text "redomapT" <>
    parens (ppr outer <> comma </> ppr inner <> comma </>
            commasep (braces (commasep $ map ppr es) : map ppr as))
  ppr (Scan cs lam inputs _) =
    ppCertificates' cs <> ppSOAC "scanT" [lam] (Just es) as
    where (es, as) = unzip inputs
  ppr (Filter cs lam as _) =
    ppCertificates' cs <> ppSOAC "filterT" [lam] Nothing as

instance PrettyLore lore => Pretty (Exp lore) where
  ppr (If c t f _ _) = text "if" <+> ppr c </>
                             text "then" <+> align (ppr t) </>
                             text "else" <+> align (ppr f)
  ppr (PrimOp op) = ppr op
  ppr (LoopOp op) = ppr op
  ppr (Apply fname args _ _) = text (nameToString fname) <>
                               apply (map (align . ppr . fst) args)

instance PrettyLore lore => Pretty (Lambda lore) where
  ppr (Lambda params body rettype _) =
    text "fn" <+> ppTuple' rettype <+>
    apply (map ppParam params) <+>
    text "=>" </> indent 2 (ppr body)

instance PrettyLore lore => Pretty (FunDec lore) where
  ppr (name, rettype, args, body, _) =
    text "fun" <+> ppResType rettype <+>
    text (nameToString name) <//>
    apply (map ppParam args) <+>
    equals </> indent 2 (ppr body)

instance PrettyLore lore => Pretty (Prog lore) where
  ppr = stack . punctuate line . map ppr . progFunctions

ppParam :: Param -> Doc
ppParam param = ppr (identType param) <+> ppr param

ppBinOp :: BinOp -> SubExp -> SubExp -> Doc
ppBinOp bop x y = ppr x <+/> text (opStr bop) <+> ppr y

ppSOAC :: Pretty fn => String -> [fn] -> Maybe [SubExp] -> [SubExp] -> Doc
ppSOAC name funs es as =
  text name <> parens (ppList funs </>
                       commasep (es' ++ map ppr as))
  where es' = maybe [] ((:[]) . ppTuple') es

ppList :: Pretty a => [a] -> Doc
ppList as = case map ppr as of
              []     -> empty
              a':as' -> foldl (</>) (a' <> comma) $ map (<> comma) as'

ppPattern :: [Ident] -> Doc
ppPattern = braces . commasep . map ppBind

ppBind :: Ident -> Doc
ppBind ident = ppr (identType ident) <+> ppr ident

ppResType :: ResType -> Doc
ppResType = braces . commasep . map ppr

ppTuple' :: Pretty a => [a] -> Doc
ppTuple' ets = braces $ commasep $ map ppr ets

ppCertificates :: Certificates -> Doc
ppCertificates [] = empty
ppCertificates cs = text "<" <> commasep (map ppr cs) <> text ">"

ppCertificates' :: Certificates -> Doc
ppCertificates' [] = empty
ppCertificates' cs = ppCertificates cs <> line

render80 :: Pretty a => a -> String
render80 = pretty 80 . ppr

-- | Prettyprint a value, wrapped to 80 characters.
ppValue :: Value -> String
ppValue = render80

-- | Prettyprint several values, wrapped to 80 characters.
ppValues :: [Value] -> String
ppValues = pretty 80 . ppTuple'

-- | Prettyprint a type, wrapped to 80 characters.
ppType :: Pretty (TypeBase shape) => TypeBase shape -> String
ppType = render80

-- | Prettyprint a body, wrapped to 80 characters.
ppBody :: PrettyLore lore => Body lore -> String
ppBody = render80

-- | Prettyprint a binding, wrapped to 80 characters.
ppBinding :: PrettyLore lore => Binding lore -> String
ppBinding = render80

-- | Prettyprint an expression, wrapped to 80 characters.
ppExp :: PrettyLore lore => Exp lore -> String
ppExp = render80

-- | Prettyprint a subexpression, wrapped to 80 characters.
ppSubExp :: SubExp -> String
ppSubExp = render80

-- | Prettyprint a lambda, wrapped to 80 characters.
ppLambda :: PrettyLore lore => Lambda lore -> String
ppLambda = render80

-- | Prettyprint a function definition, wrapped to 80 characters.
ppFun :: PrettyLore lore => FunDec lore -> String
ppFun = render80

-- | Prettyprint a list enclosed in curly braces.
ppTuple :: Pretty a => [a] -> String
ppTuple = pretty 80 . ppTuple'

-- | Prettyprint an entire Futhark program, wrapped to 80 characters.
prettyPrint :: PrettyLore lore => Prog lore -> String
prettyPrint = render80
