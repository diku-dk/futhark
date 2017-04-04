{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Futhark prettyprinter.  This module defines 'Pretty' instances
-- for the AST defined in "Language.Futhark.Syntax".
module Language.Futhark.Pretty
  ( pretty
  , prettyTuple
  , leadingOperator
  )
where

import           Data.Array
import           Data.Functor
import           Data.Hashable
import qualified Data.Map.Strict       as M
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Word

import           Prelude

import           Futhark.Util.Pretty

import           Language.Futhark.Syntax
import           Language.Futhark.Attributes

commastack :: [Doc] -> Doc
commastack = align . stack . punctuate comma

instance Pretty Value where
  ppr (PrimValue bv) = ppr bv
  ppr (ArrayValue a t)
    | [] <- elems a = text "empty" <> parens (ppr t)
    | Array{} <- t  = brackets $ commastack $ map ppr $ elems a
    | otherwise     = brackets $ commasep $ map ppr $ elems a

instance Pretty PrimValue where
  ppr (UnsignedValue (Int8Value v)) =
    text (show (fromIntegral v::Word8)) <> text "u8"
  ppr (UnsignedValue (Int16Value v)) =
    text (show (fromIntegral v::Word16)) <> text "u16"
  ppr (UnsignedValue (Int32Value v)) =
    text (show (fromIntegral v::Word32)) <> text "u32"
  ppr (UnsignedValue (Int64Value v)) =
    text (show (fromIntegral v::Word64)) <> text "u64"
  ppr (SignedValue v) = ppr v
  ppr (BoolValue True) = text "true"
  ppr (BoolValue False) = text "false"
  ppr (FloatValue v) = ppr v

instance (Eq vn, Hashable vn, Pretty vn) =>
         Pretty (RecordArrayElemTypeBase (ShapeDecl vn) as) where
  ppr (PrimArrayElem bt _ u) = ppr u <> ppr bt
  ppr (PolyArrayElem bt _ u) = ppr u <> ppr (baseName <$> qualNameFromTypeName bt)
  ppr (ArrayArrayElem at)    = ppr at
  ppr (RecordArrayElem fs)
    | Just ts <- areTupleFields fs =
        parens $ commasep $ map ppr ts
    | otherwise =
        braces $ commasep $ map ppField $ M.toList fs
    where ppField (name, t) = text (nameToString name) <> colon <> ppr t

instance Pretty (RecordArrayElemTypeBase Rank as) where
  ppr (PrimArrayElem bt _ u) = ppr u <> ppr bt
  ppr (PolyArrayElem bt _ u) = ppr u <> ppr (baseName <$> qualNameFromTypeName bt)
  ppr (ArrayArrayElem at)    = ppr at
  ppr (RecordArrayElem fs)
    | Just ts <- areTupleFields fs =
        parens $ commasep $ map ppr ts
    | otherwise =
        braces $ commasep $ map ppField $ M.toList fs
    where ppField (name, t) = text (nameToString name) <> colon <> ppr t

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (ArrayTypeBase (ShapeDecl vn) as) where
  ppr (PrimArray et (ShapeDecl ds) u _) =
    ppr u <> mconcat (map (brackets . f) ds) <> ppr et
    where f AnyDim       = mempty
          f (NamedDim v) = ppr v
          f (ConstDim n) = ppr n

  ppr (PolyArray et (ShapeDecl ds) u _) =
    ppr u <> mconcat (map (brackets . f) ds) <> ppr (baseName <$> qualNameFromTypeName et)
    where f AnyDim       = mempty
          f (NamedDim v) = ppr v
          f (ConstDim n) = ppr n

  ppr (RecordArray fs (ShapeDecl ds) u)
    | Just ts <- areTupleFields fs =
        prefix <> parens (commasep $ map ppr ts)
    | otherwise =
        prefix <> braces (commasep $ map ppField $ M.toList fs)
    where prefix =       ppr u <> mconcat (map (brackets . f) ds)
          f AnyDim       = mempty
          f (NamedDim v) = ppr v
          f (ConstDim n) = ppr n
          ppField (name, t) = text (nameToString name) <> colon <> ppr t

instance Pretty (ArrayTypeBase Rank as) where
  ppr (PrimArray et (Rank n) u _) =
    ppr u <> mconcat (replicate n (brackets mempty)) <> ppr et
  ppr (PolyArray et (Rank n) u _) =
    ppr u <> mconcat (replicate n (brackets mempty)) <> ppr (baseName <$> qualNameFromTypeName et)
  ppr (RecordArray fs (Rank n) u)
    | Just ts <- areTupleFields fs =
        prefix <> parens (commasep $ map ppr ts)
    | otherwise =
        prefix <> braces (commasep $ map ppField $ M.toList fs)
    where prefix = ppr u <> mconcat (replicate n (brackets mempty))
          ppField (name, t) = text (nameToString name) <> colon <> ppr t

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (TypeBase (ShapeDecl vn) as) where
  ppr (Prim et)    = ppr et
  ppr (TypeVar et) = ppr $ baseName <$> qualNameFromTypeName et
  ppr (Array at)   = ppr at
  ppr (Record fs)
    | Just ts <- areTupleFields fs =
        parens $ commasep $ map ppr ts
    | otherwise =
        braces $ commasep $ map ppField $ M.toList fs
    where ppField (name, t) = text (nameToString name) <> colon <> ppr t

instance Pretty (TypeBase Rank as) where
  ppr (Prim et)    = ppr et
  ppr (TypeVar et) = ppr $ baseName <$> qualNameFromTypeName et
  ppr (Array at)   = ppr at
  ppr (Record fs)
    | Just ts <- areTupleFields fs =
        parens $ commasep $ map ppr ts
    | otherwise =
      braces $ commasep $ map ppField $ M.toList fs
    where ppField (name, t) = text (nameToString name) <> colon <> ppr t

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (TypeExp vn) where
  ppr (TEUnique t _) = text "*" <> ppr t
  ppr (TEArray at d _) = brackets (f d) <> ppr at
    where f AnyDim       = mempty
          f (NamedDim v) = ppr v
          f (ConstDim n) = ppr n
  ppr (TETuple ts _) = parens $ commasep $ map ppr ts
  ppr (TERecord fs _) = braces $ commasep $ map ppField fs
    where ppField (name, t) = text (nameToString name) <> colon <> ppr t
  ppr (TEVar name _) = ppr name

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (TypeDeclBase f vn) where
  ppr = ppr . declaredType

instance Pretty vn => Pretty (QualName vn) where
  ppr (QualName names name) =
    mconcat $ punctuate (text ".") $ map ppr names ++ [ppr name]

instance Pretty vn => Pretty (IdentBase f vn) where
  ppr = ppr . identName

hasArrayLit :: ExpBase ty vn -> Bool
hasArrayLit ArrayLit{}     = True
hasArrayLit (TupLit es2 _) = any hasArrayLit es2
hasArrayLit _              = False

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (DimIndexBase ty vn) where
  ppr (DimFix e)       = ppr e
  ppr (DimSlice i j s) = maybe mempty ppr i <> text ":" <>
                         maybe mempty ppr j <> text ":" <>
                         maybe mempty ppr s

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (ExpBase ty vn) where
  ppr = pprPrec (-1)
  pprPrec _ (Var name _ _) = ppr name
  pprPrec _ (Parens e _) = align $ parens $ ppr e
  pprPrec _ (Ascript e t _) = pprPrec 0 e <> pprPrec 0 t
  pprPrec _ (Literal v _) = ppr v
  pprPrec _ (TupLit es _)
    | any hasArrayLit es = parens $ commastack $ map ppr es
    | otherwise          = parens $ commasep $ map ppr es
  pprPrec _ (RecordLit fs _)
    | any (hasArrayLit . recExp) fs = braces $ commastack $ map ppr fs
    | otherwise                     = braces $ commasep $ map ppr fs
    where recExp (RecordField _ e _) = e
          recExp (RecordRecord e) = e
  pprPrec _ (Empty (TypeDecl t _) _) =
    text "empty" <> parens (ppr t)
  pprPrec _ (ArrayLit es _ _) =
    brackets $ commasep $ map ppr es
  pprPrec p (BinOp bop (x,_) (y,_) _ _) = prettyBinOp p bop x y
  pprPrec _ (Project k e _ _) = text "#" <> ppr k <+> pprPrec 9 e
  pprPrec _ (If c t f _ _) = text "if" <+> ppr c </>
                             text "then" <+> align (ppr t) </>
                             text "else" <+> align (ppr f)
  pprPrec _ (Apply fname args _ _) = ppr fname <+>
                                     spread (map (ppr . fst) args)
  pprPrec _ (Negate e _) = text "-" <> ppr e
  pprPrec p (LetPat pat e body _) =
    mparens $ align $
    text "let" <+> align (ppr pat) <+>
    (if linebreak
     then equals </> indent 2 (ppr e)
     else equals <+> align (ppr e)) <+> text "in" </>
    ppr body
    where mparens = if p == -1 then id else parens
          linebreak = case e of
                        Map{}      -> True
                        Reduce{}   -> True
                        Filter{}   -> True
                        Scan{}     -> True
                        DoLoop{}   -> True
                        LetPat{}   -> True
                        LetWith{}  -> True
                        If{}       -> True
                        ArrayLit{} -> False
                        _          -> hasArrayLit e
  pprPrec _ (LetFun fname (params, retdecl, _, e) body _) =
    text "let" <+> ppr fname <+> spread (map ppr params) <> retdecl' <+> equals </>
    indent 2 (ppr e) <+> text "in" </>
    ppr body
    where retdecl' = case retdecl of
                       Just rettype -> text ":" <+> ppr rettype
                       Nothing      -> mempty
  pprPrec _ (LetWith dest src idxs ve body _)
    | dest == src =
      text "let" <+> ppr dest <+> list (map ppr idxs) <+>
      equals <+> align (ppr ve) <+>
      text "in" </> ppr body
    | otherwise =
      text "let" <+> ppr dest <+> equals <+> ppr src <+>
      text "with" <+> brackets (commasep (map ppr idxs)) <+>
      text "<-" <+> align (ppr ve) <+>
      text "in" </> ppr body
  pprPrec _ (Update src idxs ve _) =
    ppr src <+> text "with" <+>
    brackets (commasep (map ppr idxs)) <+>
    text "<-" <+> align (ppr ve)
  pprPrec _ (Index e idxs _) =
    pprPrec 9 e <> brackets (commasep (map ppr idxs))
  pprPrec _ (Iota e _) = text "iota" <+> pprPrec 10 e
  pprPrec _ (Shape e _) =
    text "shape" <+> pprPrec 10 e
  pprPrec _ (Replicate ne ve _) =
    text "replicate" <+> spread [pprPrec 10 ne, pprPrec 10 ve]
  pprPrec _ (Reshape shape e _) =
    text "reshape" <+> ppr shape <+> ppr e
  pprPrec _ (Rearrange perm e _) =
    text "rearrange" <> apply [apply (map ppr perm), ppr e]
  pprPrec _ (Transpose e _) =
    text "transpose" <> apply [ppr e]
  pprPrec _ (Rotate d x e _) =
    text "rotate@" <> ppr d <> apply [ppr x, ppr e]
  pprPrec _ (Map lam as _) = ppSOAC "map" [lam] as
  pprPrec _ (Reduce Commutative lam e a _) = ppSOAC "reduce_comm" [lam] [e, a]
  pprPrec _ (Reduce Noncommutative lam e a _) = ppSOAC "reduce" [lam] [e, a]
  pprPrec _ (Stream form lam arr _) =
    case form of
      MapLike o ->
        let ord_str = if o == Disorder then "_per" else ""
        in  text ("stream_map"++ord_str) <>
            ppr lam </> pprPrec 10 arr
      RedLike o comm lam0 ->
        let ord_str = if o == Disorder then "_per" else ""
            comm_str = case comm of Commutative    -> "_comm"
                                    Noncommutative -> ""
        in  text ("stream_red"++ord_str++comm_str) <>
            ppr lam0 </> ppr lam </> pprPrec 10 arr
      Sequential acc ->
            text "stream_seq" <+>
            ppr lam </> spread [pprPrec 10 acc, pprPrec 10 arr]
  pprPrec _ (Scan lam e a _) = ppSOAC "scan" [lam] [e, a]
  pprPrec _ (Filter lam a _) = ppSOAC "filter" [lam] [a]
  pprPrec _ (Partition lams a _) = ppSOAC "partition" lams [a]
  pprPrec _ (Zip 0 e es _) = text "zip" <+> spread (map (pprPrec 10) (e:es))
  pprPrec _ (Zip i e es _) = text "zip@" <> ppr i <+> spread (map (pprPrec 10) (e:es))
  pprPrec _ (Unzip e _ _) = text "unzip" <> pprPrec 10 e
  pprPrec _ (Unsafe e _) = text "unsafe" <+> pprPrec 10 e
  pprPrec _ (Split i e a _) =
    text "split@" <> ppr i <+> pprPrec 10 e <+> pprPrec 10 a
  pprPrec _ (Concat i x y _) =
    text "concat" <> text "@" <> ppr i <+> pprPrec 10 x <+> pprPrec 10 y
  pprPrec _ (Copy e _) = text "copy" <> pprPrec 10 e
  pprPrec _ (DoLoop pat initexp form loopbody letbody _) =
    text "loop" <+> parens (ppr pat <+> equals <+> ppr initexp) <+> equals <+>
    ppr form <+>
    text "do" </>
    indent 2 (ppr loopbody) <+> text "in" </>
    ppr letbody
  pprPrec _ (Scatter i v a _) = text "scatter" <> spread [pprPrec 10 i, pprPrec 10 v, pprPrec 10 a]

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (FieldBase ty vn) where
  ppr (RecordField name e _) = ppr name <> equals <> ppr e
  ppr (RecordRecord e) = ppr e

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (LoopFormBase ty vn) where
  ppr (For FromUpTo lbound i ubound) =
    text "for" <+> align (ppr lbound) <+> ppr i <+> text "<" <+> align (ppr ubound)
  ppr (For FromDownTo lbound i ubound) =
    text "for" <+> align (ppr ubound) <+> ppr i <+> text ">" <+> align (ppr lbound)
  ppr (While cond) =
    text "while" <+> ppr cond

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (LowerBoundBase ty vn) where
  ppr ZeroBound    = text "0"
  ppr (ExpBound e) = ppr e

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (PatternBase ty vn) where
  ppr (PatternAscription p t) = ppr p <> text ":" <+> ppr t
  ppr (PatternParens p _)     = parens $ ppr p
  ppr (Id ident)              = ppr ident
  ppr (TuplePattern pats _)   = parens $ commasep $ map ppr pats
  ppr (RecordPattern fs _)    = braces $ commasep $ map ppField fs
    where ppField (name, t) = text (nameToString name) <> equals <> ppr t
  ppr (Wildcard _ _)          = text "_"

ppAscription :: (Eq vn, Hashable vn, Pretty vn) => Maybe (TypeDeclBase ty vn) -> Doc
ppAscription Nothing  = mempty
ppAscription (Just t) = text ":" <> ppr t

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (LambdaBase ty vn) where
  ppr (CurryFun fname [] _ _) = text $ pretty fname
  ppr (CurryFun fname curryargs _ _) =
    ppr fname <+> apply (map ppr curryargs)
  ppr (AnonymFun params body ascript _ _) =
    text "fn" <+>
    apply (map ppr params) <> ppAscription ascript <+>
    text "=>" </> indent 2 (ppr body)
  ppr (BinOpFun binop _ _ _ _) =
    ppr binop
  ppr (CurryBinOpLeft binop x _ _ _) =
    ppr x <+> ppr binop
  ppr (CurryBinOpRight binop x _ _ _) =
    ppr binop <+> ppr x

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (ProgBase ty vn) where
  ppr = stack . punctuate line . map ppr . progDecs

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (DecBase ty vn) where
  ppr (ValDec dec)     = ppr dec
  ppr (FunDec dec)     = ppr dec
  ppr (TypeDec dec)    = ppr dec
  ppr (SigDec sig)     = ppr sig
  ppr (ModDec sd)      = ppr sd
  ppr (OpenDec x xs _) = text "open" <+> spread (map ppr (x:xs))

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (ModExpBase ty vn) where
  ppr (ModVar v _) = ppr v
  ppr (ModParens e _) = parens $ ppr e
  ppr (ModImport v _) = ppr $ show v
  ppr (ModDecs ds _) = nestedBlock "{" "}" (stack $ punctuate line $ map ppr ds)
  ppr (ModApply f a _ _ _) = parens $ ppr f <+> parens (ppr a)
  ppr (ModAscript me se _ _) = ppr me <> colon <+> ppr se
  ppr (ModLambda param maybe_sig body _) =
    text "\\" <> ppr param <> maybe_sig' <+>
    text "->" </> indent 2 (ppr body)
    where maybe_sig' = case maybe_sig of Nothing  -> mempty
                                         Just sig -> colon <+> ppr sig

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (TypeBindBase ty vn) where
  ppr (TypeBind name usertype _) =
    text "type" <+> ppr name <+> equals <+> ppr usertype

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (FunBindBase ty vn) where
  ppr (FunBind entry name retdecl _ args body _) =
    text fun <+> ppr name <+>
    spread (map ppr args) <> retdecl' <+> equals </>
    indent 2 (ppr body)
    where fun | entry     = "entry"
              | otherwise = "let"
          retdecl' = case retdecl of
                       Just rettype -> text ":" <+> ppr rettype
                       Nothing      -> mempty

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (ValBindBase ty vn) where
  ppr (ValBind name maybe_t _ e _) =
    text "let" <+> ppr name <> t' <+> text "=" <+> ppr e
    where t' = case maybe_t of Just t  -> text ":" <+> ppr t
                               Nothing -> mempty

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (SpecBase ty vn) where
  ppr (TypeAbbrSpec tpsig) = ppr tpsig
  ppr (TypeSpec name _) = text "type" <+> ppr name
  ppr (ValSpec name params rettype _) =
    text "val" <+> ppr name <> colon <+>
    mconcat (map (\p -> ppr p <+> text "-> ") params) <+> ppr rettype
  ppr (ModSpec name sig _) =
    text "module" <+> ppr name <> colon <+> ppr sig
  ppr (IncludeSpec e _) =
    text "include" <+> ppr e

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (SigExpBase ty vn) where
  ppr (SigVar v _) = ppr v
  ppr (SigParens e _) = parens $ ppr e
  ppr (SigSpecs ss _) = nestedBlock "{" "}" (stack $ punctuate line $ map ppr ss)
  ppr (SigWith s (TypeRef v td) _) =
    ppr s <+> text "with" <+> ppr v <+> equals <+> ppr td
  ppr (SigArrow (Just v) e1 e2 _) =
    parens (ppr v <> colon <+> ppr e1) <+> text "->" <+> ppr e2
  ppr (SigArrow Nothing e1 e2 _) =
    ppr e1 <+> text "->" <+> ppr e2

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (SigBindBase ty vn) where
  ppr (SigBind name e _) =
    text "module type" <+> ppr name <+> equals <+> ppr e

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (ModParamBase ty vn) where
  ppr (ModParam pname psig _) =
    parens (ppr pname <> colon <+> ppr psig)

instance (Eq vn, Hashable vn, Pretty vn) => Pretty (ModBindBase ty vn) where
  ppr (ModBind name ps sig e _) =
    text "module" <+> ppr name <+> spread (map ppr ps) <+> sig' <+> equals <+> ppr e
    where sig' = case sig of Nothing    -> mempty
                             Just (s,_) -> colon <+> ppr s <> text " "

prettyBinOp :: (Eq vn, Hashable vn, Pretty vn) =>
               Int -> QualName vn -> ExpBase ty vn -> ExpBase ty vn -> Doc
prettyBinOp p bop x y = parensIf (p > symPrecedence bop) $
                        pprPrec (symPrecedence bop) x <+/>
                        ppr bop <+>
                        pprPrec (symRPrecedence bop) y
  where symPrecedence = precedence . leadingOperator . nameFromString . pretty
        symRPrecedence = rprecedence . leadingOperator . nameFromString . pretty
        precedence LogAnd   = 0
        precedence LogOr    = 0
        precedence Band     = 1
        precedence Bor      = 1
        precedence Xor      = 1
        precedence Equal    = 2
        precedence NotEqual = 2
        precedence Less     = 2
        precedence Leq      = 2
        precedence Greater  = 2
        precedence Geq      = 2
        precedence ShiftL   = 3
        precedence ShiftR   = 3
        precedence ZShiftR  = 3
        precedence Plus     = 4
        precedence Minus    = 4
        precedence Times    = 5
        precedence Divide   = 5
        precedence Mod      = 5
        precedence Quot     = 5
        precedence Rem      = 5
        precedence Pow      = 6
        rprecedence Minus  = 10
        rprecedence Divide = 10
        rprecedence op     = precedence op

ppSOAC :: (Eq vn, Hashable vn, Pretty vn, Pretty fn) =>
          String -> [fn] -> [ExpBase ty vn] -> Doc
ppSOAC name funs es =
  text name <+> spread (map ppr funs) </> spread (map (pprPrec 10) es)
