{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
-- | Futhark prettyprinter.  This module defines 'Pretty' instances for the
-- AST defined in "Futhark.InternalRep.Syntax", but also a number of
-- convenience functions if you don't want to use the interface from
-- 'Pretty'.
module Futhark.InternalRep.Pretty
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
  )
  where

import Data.Array
import qualified Data.HashSet as HS

import Text.PrettyPrint.Mainland

import Futhark.InternalRep.Syntax
import Futhark.InternalRep.Attributes

-- | The document @'apply' ds@ separates @ds@ with commas and encloses them with
-- parentheses.
apply :: [Doc] -> Doc
apply = encloseSep lparen rparen comma . map align

commastack :: [Doc] -> Doc
commastack = align . stack . punctuate comma

aliasComment :: [Ident] -> Doc -> Doc
aliasComment pat d = case concatMap aliasComment' pat of
                       []   -> d
                       l:ls -> foldl (</>) l ls </> d
  where aliasComment' ident =
          case (clean . HS.toList . aliases) $ identType ident of
            [] -> []
            als -> [oneline $
                    text "// " <> ppr ident <> text " aliases " <>
                    commasep (map ppr als)]
          where clean = filter (/= identName ident)
                oneline s = text $ displayS (renderCompact s) ""

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

instance Pretty (TypeBase als Shape) where
  ppr (Basic et) = ppr et
  ppr (Array et (Shape ds) u _) = ppr u <> foldr f (ppr et) ds
    where f e s = brackets $ s <> comma <> ppr e

instance Pretty (TypeBase als ExtShape) where
  ppr (Basic et) = ppr et
  ppr (Array et (ExtShape ds) u _) = ppr u <> foldr f (ppr et) ds
    where f (Free e) s = brackets $ s <> comma <> ppr e
          f (Ext x)  s = brackets $ s <> comma <>
                         text "<" <> text (show x) <> text ">"

instance Pretty (TypeBase als Rank) where
  ppr (Basic et) = ppr et
  ppr (Array et (Rank n) u _) = u' <> foldl f (ppr et) [1..n]
    where f s _ = brackets s
          u' | Unique <- u = star
             | otherwise = empty

instance Pretty (IdentBase als shape) where
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

instance Pretty Body where
  ppr (Body (bnd:bnds) res) =
    ppr bnd <+> text "in" </> ppr (Body bnds res)
  ppr (Body [] (Result cs es _))
    | any hasArrayLit es = ppCertificates cs <> braces (commastack $ map ppr es)
    | otherwise          = ppCertificates cs <> braces (commasep   $ map ppr es)

instance Pretty Binding where
  ppr (Let pat e) =
    aliasComment pat $ align $
    text "let" <+> align (ppPattern pat) <+>
    (if linebreak
     then equals </> indent 2 (ppr e)
     else equals <+> align (ppr e))
    where linebreak = case e of
                        DoLoop {} -> True
                        Map {} -> True
                        Reduce {} -> True
                        Filter {} -> True
                        Redomap {} -> True
                        Scan {} -> True
                        SubExp (Constant (ArrayVal {}) _) -> False
                        If {} -> True
                        ArrayLit {} -> False
                        _ -> False

instance Pretty Exp where
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
  ppr (If c t f _ _) = text "if" <+> ppr c </>
                             text "then" <+> align (ppr t) </>
                             text "else" <+> align (ppr f)
  ppr (Apply fname args _ _) = text (nameToString fname) <>
                                     apply (map (align . ppr . fst) args)

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
  ppr (Filter cs lam as _ _) =
    ppCertificates' cs <> ppSOAC "filterT" [lam] Nothing as

instance Pretty Lambda where
  ppr (Lambda params body rettype _) =
    text "fn" <+> ppTuple' rettype <+>
    apply (map ppParam params) <+>
    text "=>" </> indent 2 (ppr body)

instance Pretty FunDec where
  ppr (name, rettype, args, body, _) =
    text "fun" <+> ppRetType rettype <+>
    text (nameToString name) <//>
    apply (map ppParam args) <+>
    equals </> indent 2 (ppr body)

instance Pretty Prog where
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

ppRetType :: RetType -> Doc
ppRetType = braces . commasep . map ppr

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
ppType :: Pretty (TypeBase als shape) => TypeBase als shape -> String
ppType = render80

-- | Prettyprint a body, wrapped to 80 characters.
ppBody :: Body -> String
ppBody = render80

-- | Prettyprint a binding, wrapped to 80 characters.
ppBinding :: Binding -> String
ppBinding = render80

-- | Prettyprint an expression, wrapped to 80 characters.
ppExp :: Exp -> String
ppExp = render80

-- | Prettyprint a subexpression, wrapped to 80 characters.
ppSubExp :: SubExp -> String
ppSubExp = render80

-- | Prettyprint a lambda, wrapped to 80 characters.
ppLambda :: Lambda -> String
ppLambda = render80

-- | Prettyprint a function definition, wrapped to 80 characters.
ppFun :: FunDec -> String
ppFun = render80

-- | Prettyprint a list enclosed in curly braces.
ppTuple :: Pretty a => [a] -> String
ppTuple = pretty 80 . ppTuple'

-- | Prettyprint an entire Futhark program, wrapped to 80 characters.
prettyPrint :: Prog -> String
prettyPrint = render80
