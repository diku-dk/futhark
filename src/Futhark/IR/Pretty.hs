{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Futhark prettyprinter.  This module defines 'Pretty' instances
-- for the AST defined in "Futhark.IR.Syntax",
-- but also a number of convenience functions if you don't want to use
-- the interface from 'Pretty'.
module Futhark.IR.Pretty
  ( prettyTuple,
    prettyTupleLines,
    prettyString,
    PrettyRep (..),
  )
where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe
import Futhark.IR.Syntax
import Futhark.Util.Pretty

-- | The class of representations whose annotations can be prettyprinted.
class
  ( RepTypes rep,
    Pretty (RetType rep),
    Pretty (BranchType rep),
    Pretty (FParamInfo rep),
    Pretty (LParamInfo rep),
    Pretty (LetDec rep),
    Pretty (Op rep)
  ) =>
  PrettyRep rep
  where
  ppExpDec :: ExpDec rep -> Exp rep -> Maybe (Doc a)
  ppExpDec _ _ = Nothing

instance Pretty (NoOp rep) where
  pretty NoOp = "noop"

instance Pretty VName where
  pretty (VName vn i) = pretty vn <> "_" <> pretty (show i)

instance Pretty Commutativity where
  pretty Commutative = "commutative"
  pretty Noncommutative = "noncommutative"

instance Pretty Shape where
  pretty = mconcat . map (brackets . pretty) . shapeDims

instance Pretty Rank where
  pretty (Rank r) = mconcat $ replicate r "[]"

instance (Pretty a) => Pretty (Ext a) where
  pretty (Free e) = pretty e
  pretty (Ext x) = "?" <> pretty (show x)

instance Pretty ExtShape where
  pretty = mconcat . map (brackets . pretty) . shapeDims

instance Pretty Space where
  pretty DefaultSpace = mempty
  pretty (Space s) = "@" <> pretty s
  pretty (ScalarSpace d t) = "@" <> mconcat (map (brackets . pretty) d) <> pretty t

instance (Pretty u) => Pretty (TypeBase Shape u) where
  pretty (Prim t) = pretty t
  pretty (Acc acc ispace ts u) =
    pretty u
      <> "acc"
      <> apply
        [ pretty acc,
          pretty ispace,
          ppTuple' $ map pretty ts
        ]
  pretty (Array et (Shape ds) u) =
    pretty u <> mconcat (map (brackets . pretty) ds) <> pretty et
  pretty (Mem s) = "mem" <> pretty s

instance (Pretty u) => Pretty (TypeBase ExtShape u) where
  pretty (Prim t) = pretty t
  pretty (Acc acc ispace ts u) =
    pretty u
      <> "acc"
      <> apply
        [ pretty acc,
          pretty ispace,
          ppTuple' $ map pretty ts
        ]
  pretty (Array et (Shape ds) u) =
    pretty u <> mconcat (map (brackets . pretty) ds) <> pretty et
  pretty (Mem s) = "mem" <> pretty s

instance (Pretty u) => Pretty (TypeBase Rank u) where
  pretty (Prim t) = pretty t
  pretty (Acc acc ispace ts u) =
    pretty u
      <> "acc"
      <> apply
        [ pretty acc,
          pretty ispace,
          ppTuple' $ map pretty ts
        ]
  pretty (Array et (Rank n) u) =
    pretty u <> mconcat (replicate n $ brackets mempty) <> pretty et
  pretty (Mem s) = "mem" <> pretty s

instance Pretty Ident where
  pretty ident = pretty (identType ident) <+> pretty (identName ident)

instance Pretty SubExp where
  pretty (Var v) = pretty v
  pretty (Constant v) = pretty v

instance Pretty Certs where
  pretty (Certs []) = mempty
  pretty (Certs cs) = "#" <> braces (commasep (map pretty cs))

instance (PrettyRep rep) => Pretty (Stms rep) where
  pretty = stack . map pretty . stmsToList

instance Pretty SubExpRes where
  pretty (SubExpRes cs se) = hsep $ certAnnots cs ++ [pretty se]

instance (PrettyRep rep) => Pretty (Body rep) where
  pretty (Body _ stms res)
    | null stms = braces (commasep $ map pretty res)
    | otherwise =
        stack (map pretty $ stmsToList stms)
          </> "in"
          <+> braces (commasep $ map pretty res)

instance Pretty Attr where
  pretty (AttrName v) = pretty v
  pretty (AttrInt x) = pretty x
  pretty (AttrComp f attrs) = pretty f <> parens (commasep $ map pretty attrs)

attrAnnots :: Attrs -> [Doc a]
attrAnnots = map f . toList . unAttrs
  where
    f v = "#[" <> pretty v <> "]"

stmAttrAnnots :: Stm rep -> [Doc a]
stmAttrAnnots = attrAnnots . stmAuxAttrs . stmAux

certAnnots :: Certs -> [Doc a]
certAnnots cs
  | cs == mempty = []
  | otherwise = [pretty cs]

stmCertAnnots :: Stm rep -> [Doc a]
stmCertAnnots = certAnnots . stmAuxCerts . stmAux

instance Pretty Attrs where
  pretty = hsep . attrAnnots

instance (Pretty t) => Pretty (Pat t) where
  pretty (Pat xs) = braces $ commastack $ map pretty xs

instance (Pretty t) => Pretty (PatElem t) where
  pretty (PatElem name t) = pretty name <+> colon <+> align (pretty t)

instance (Pretty t) => Pretty (Param t) where
  pretty (Param attrs name t) =
    annot (attrAnnots attrs) $ pretty name <+> colon <+> align (pretty t)

instance (PrettyRep rep) => Pretty (Stm rep) where
  pretty stm@(Let pat aux e) =
    align . hang 2 $
      "let"
        <+> align (pretty pat)
        <+> case stmannot of
          [] -> equals </> pretty e
          _ -> equals </> (stack stmannot </> pretty e)
    where
      stmannot =
        concat
          [ maybeToList (ppExpDec (stmAuxDec aux) e),
            stmAttrAnnots stm,
            stmCertAnnots stm
          ]

instance (Pretty a) => Pretty (Slice a) where
  pretty (Slice xs) = brackets (commasep (map pretty xs))

instance (Pretty d) => Pretty (FlatDimIndex d) where
  pretty (FlatDimIndex n s) = pretty n <+> ":" <+> pretty s

instance (Pretty a) => Pretty (FlatSlice a) where
  pretty (FlatSlice offset xs) = brackets (pretty offset <> ";" <+> commasep (map pretty xs))

instance Pretty BasicOp where
  pretty (SubExp se) = pretty se
  pretty (Opaque OpaqueNil e) = "opaque" <> apply [pretty e]
  pretty (Opaque (OpaqueTrace s) e) = "trace" <> apply [pretty (show s), pretty e]
  pretty (ArrayLit es rt) =
    case rt of
      Array {} -> brackets $ commastack $ map pretty es
      _ -> brackets $ commasep $ map pretty es
      <+> colon
      <+> "[]"
      <> pretty rt
  pretty (BinOp bop x y) = pretty bop <> parens (pretty x <> comma <+> pretty y)
  pretty (CmpOp op x y) = pretty op <> parens (pretty x <> comma <+> pretty y)
  pretty (ConvOp conv x) =
    pretty (convOpFun conv) <+> pretty fromtype <+> pretty x <+> "to" <+> pretty totype
    where
      (fromtype, totype) = convOpType conv
  pretty (UnOp op e) = pretty op <+> pretty e
  pretty (Index v slice) = pretty v <> pretty slice
  pretty (Update safety src slice se) =
    pretty src <+> with <+> pretty slice <+> "=" <+> pretty se
    where
      with = case safety of
        Unsafe -> "with"
        Safe -> "with?"
  pretty (FlatIndex v slice) = pretty v <> pretty slice
  pretty (FlatUpdate src slice se) =
    pretty src <+> "with" <+> pretty slice <+> "=" <+> pretty se
  pretty (Iota e x s et) = "iota" <> et' <> apply [pretty e, pretty x, pretty s]
    where
      et' = pretty $ show $ primBitSize $ IntType et
  pretty (Replicate (Shape []) e) = "copy" <> parens (pretty e)
  pretty (Replicate ne ve) =
    "replicate" <> apply [pretty ne, align (pretty ve)]
  pretty (Scratch t shape) =
    "scratch" <> apply (pretty t : map pretty shape)
  pretty (Reshape ReshapeArbitrary shape e) =
    "reshape" <> apply [pretty shape, pretty e]
  pretty (Reshape ReshapeCoerce shape e) =
    "coerce" <> apply [pretty shape, pretty e]
  pretty (Rearrange perm e) =
    "rearrange" <> apply [apply (map pretty perm), pretty e]
  pretty (Concat i (x :| xs) w) =
    "concat" <> "@" <> pretty i <> apply (pretty w : pretty x : map pretty xs)
  pretty (Manifest perm e) = "manifest" <> apply [apply (map pretty perm), pretty e]
  pretty (Assert e msg (loc, _)) =
    "assert" <> apply [pretty e, pretty msg, pretty $ show $ locStr loc]
  pretty (UpdateAcc acc is v) =
    "update_acc"
      <> apply
        [ pretty acc,
          ppTuple' $ map pretty is,
          ppTuple' $ map pretty v
        ]

instance (Pretty a) => Pretty (ErrorMsg a) where
  pretty (ErrorMsg parts) = braces $ align $ commasep $ map p parts
    where
      p (ErrorString s) = pretty $ show s
      p (ErrorVal t x) = pretty x <+> colon <+> pretty t

maybeNest :: (PrettyRep rep) => Body rep -> Doc a
maybeNest b
  | null $ bodyStms b = pretty b
  | otherwise = nestedBlock "{" "}" $ pretty b

instance (PrettyRep rep) => Pretty (Case (Body rep)) where
  pretty (Case vs b) =
    "case" <+> ppTuple' (map (maybe "_" pretty) vs) <+> "->" <+> maybeNest b

prettyRet :: (Pretty t) => (t, RetAls) -> Doc a
prettyRet (t, RetAls pals rals)
  | pals == mempty,
    rals == mempty =
      pretty t
  | otherwise =
      pretty t <> "#" <> parens (pl pals <> comma <+> pl rals)
  where
    pl = brackets . commasep . map pretty

instance (PrettyRep rep) => Pretty (Exp rep) where
  pretty (Match [c] [Case [Just (BoolValue True)] t] f (MatchDec ret ifsort)) =
    "if"
      <> info'
        <+> pretty c
        </> "then"
        <+> maybeNest t
        <+> "else"
        <+> maybeNest f
        </> colon
        <+> ppTupleLines' (map pretty ret)
    where
      info' = case ifsort of
        MatchNormal -> mempty
        MatchFallback -> " <fallback>"
        MatchEquiv -> " <equiv>"
  pretty (Match ses cs defb (MatchDec ret ifsort)) =
    ("match" <+> info' <+> ppTuple' (map pretty ses))
      </> stack (map pretty cs)
      </> "default"
      <+> "->"
      <+> maybeNest defb
      </> colon
      <+> ppTupleLines' (map pretty ret)
    where
      info' = case ifsort of
        MatchNormal -> mempty
        MatchFallback -> " <fallback>"
        MatchEquiv -> " <equiv>"
  pretty (BasicOp op) = pretty op
  pretty (Apply fname args ret (safety, _, _)) =
    applykw
      <+> pretty (nameToString fname)
      <> apply (map (align . prettyArg) args)
        </> colon
        <+> braces (commasep $ map prettyRet ret)
    where
      prettyArg (arg, Consume) = "*" <> pretty arg
      prettyArg (arg, _) = pretty arg
      applykw = case safety of
        Unsafe -> "apply <unsafe>"
        Safe -> "apply"
  pretty (Op op) = pretty op
  pretty (Loop merge form loopbody) =
    "loop"
      <+> braces (commastack $ map pretty params)
      <+> equals
      <+> ppTuple' (map pretty args)
      </> ( case form of
              ForLoop i it bound ->
                "for"
                  <+> align
                    ( pretty i
                        <> ":"
                        <> pretty it
                          <+> "<"
                          <+> align (pretty bound)
                    )
              WhileLoop cond ->
                "while" <+> pretty cond
          )
      <+> "do"
      <+> nestedBlock "{" "}" (pretty loopbody)
    where
      (params, args) = unzip merge
  pretty (WithAcc inputs lam) =
    "with_acc"
      <> parens (braces (commastack $ map ppInput inputs) <> comma </> pretty lam)
    where
      ppInput (shape, arrs, op) =
        parens
          ( pretty shape
              <> comma
                <+> ppTuple' (map pretty arrs)
              <> case op of
                Nothing -> mempty
                Just (op', nes) ->
                  comma </> parens (pretty op' <> comma </> ppTuple' (map pretty nes))
          )

instance (PrettyRep rep) => Pretty (Lambda rep) where
  pretty (Lambda [] [] (Body _ stms [])) | stms == mempty = "nilFn"
  pretty (Lambda params rettype body) =
    "\\"
      <+> braces (commastack $ map pretty params)
      </> indent 2 (colon <+> ppTupleLines' (map pretty rettype) <+> "->")
      </> indent 2 (pretty body)

instance Pretty Signedness where
  pretty Signed = "signed"
  pretty Unsigned = "unsigned"

instance Pretty ValueType where
  pretty (ValueType s (Rank r) t) =
    mconcat (replicate r "[]") <> pretty (prettySigned (s == Unsigned) t)

instance Pretty EntryPointType where
  pretty (TypeTransparent t) = pretty t
  pretty (TypeOpaque desc) = "opaque" <+> dquotes (pretty desc)

instance Pretty EntryParam where
  pretty (EntryParam name u t) = pretty name <> colon <+> pretty u <> pretty t

instance Pretty EntryResult where
  pretty (EntryResult u t) = pretty u <> pretty t

instance (PrettyRep rep) => Pretty (FunDef rep) where
  pretty (FunDef entry attrs name rettype fparams body) =
    annot (attrAnnots attrs) $
      fun
        </> indent 2 (pretty (nameToString name))
        <+> parens (commastack $ map pretty fparams)
        </> indent 2 (colon <+> align (ppTupleLines' $ map prettyRet rettype))
        <+> equals
        <+> nestedBlock "{" "}" (pretty body)
    where
      fun = case entry of
        Nothing -> "fun"
        Just (p_name, p_entry, ret_entry) ->
          "entry"
            <> (parens . align)
              ( "\""
                  <> pretty p_name
                  <> "\""
                  <> comma
                    </> ppTupleLines' (map pretty p_entry)
                  <> comma
                    </> ppTupleLines' (map pretty ret_entry)
              )

instance Pretty OpaqueType where
  pretty (OpaqueType ts) =
    "opaque" <+> nestedBlock "{" "}" (stack $ map pretty ts)
  pretty (OpaqueRecord fs) =
    "record" <+> nestedBlock "{" "}" (stack $ map p fs)
    where
      p (f, et) = pretty f <> ":" <+> pretty et
  pretty (OpaqueSum ts cs) =
    "sum" <+> nestedBlock "{" "}" (stack $ pretty ts : map p cs)
    where
      p (c, ets) = hsep $ "#" <> pretty c : map pretty ets

instance Pretty OpaqueTypes where
  pretty (OpaqueTypes ts) = "types" <+> nestedBlock "{" "}" (stack $ map p ts)
    where
      p (name, t) = "type" <+> dquotes (pretty name) <+> equals <+> pretty t

instance (PrettyRep rep) => Pretty (Prog rep) where
  pretty (Prog types consts funs) =
    stack $ punctuate line $ pretty types : pretty consts : map pretty funs

instance (Pretty d) => Pretty (DimIndex d) where
  pretty (DimFix i) = pretty i
  pretty (DimSlice i n s) = pretty i <+> ":+" <+> pretty n <+> "*" <+> pretty s
