{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Futhark prettyprinter.  This module defines 'Pretty' instances
-- for the AST defined in "Language.Futhark.Syntax".
module Language.Futhark.Pretty
  ( pretty,
    prettyTuple,
    leadingOperator,
    IsName (..),
    prettyName,
    Annot (..),
  )
where

import Codec.Binary.UTF8.String (decode)
import Control.Monad
import Data.Array
import Data.Functor
import Data.List (intersperse)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid hiding (Sum)
import Data.Ord
import Data.Word
import Futhark.Util
import Futhark.Util.Pretty
import Language.Futhark.Prop
import Language.Futhark.Syntax
import Prelude

commastack :: [Doc] -> Doc
commastack = align . stack . punctuate comma

-- | A class for types that are variable names in the Futhark source
-- language.  This is used instead of a mere 'Pretty' instance because
-- in the compiler frontend we want to print VNames differently
-- depending on whether the FUTHARK_COMPILER_DEBUGGING environment
-- variable is set, yet in the backend we want to always print VNames
-- with the tag.  To avoid erroneously using the 'Pretty' instance for
-- VNames, we in fact only define it inside the modules for the core
-- language (as an orphan instance).
class IsName v where
  pprName :: v -> Doc

-- | Depending on the environment variable FUTHARK_COMPILER_DEBUGGING,
-- VNames are printed as either the name with an internal tag, or just
-- the base name.
instance IsName VName where
  pprName
    | isEnvVarSet "FUTHARK_COMPILER_DEBUGGING" False =
      \(VName vn i) -> ppr vn <> text "_" <> text (show i)
    | otherwise = ppr . baseName

instance IsName Name where
  pprName = ppr

-- | Prettyprint a name to a string.
prettyName :: IsName v => v -> String
prettyName = prettyDoc 80 . pprName

-- | Class for type constructors that represent annotations.  Used in
-- the prettyprinter to either print the original AST, or the computed
-- decoration.
class Annot f where
  -- | Extract value, if any.
  unAnnot :: f a -> Maybe a

instance Annot NoInfo where
  unAnnot = const Nothing

instance Annot Info where
  unAnnot = Just . unInfo

pprAnnot :: (Annot f, Pretty a, Pretty b) => a -> f b -> Doc
pprAnnot a b = maybe (ppr a) ppr $ unAnnot b

instance Pretty Value where
  pretty (PrimValue bv) = pretty bv
  pretty (ArrayValue a t)
    | [] <- elems a = text "empty" <> parens (pretty t)
    | Array {} <- t = brackets $ commastack $ map pretty $ elems a
    | otherwise = brackets $ commasep $ map pretty $ elems a

instance Pretty PrimValue where
  pretty (UnsignedValue (Int8Value v)) =
    text (show (fromIntegral v :: Word8)) <> text "u8"
  pretty (UnsignedValue (Int16Value v)) =
    text (show (fromIntegral v :: Word16)) <> text "u16"
  pretty (UnsignedValue (Int32Value v)) =
    text (show (fromIntegral v :: Word32)) <> text "u32"
  pretty (UnsignedValue (Int64Value v)) =
    text (show (fromIntegral v :: Word64)) <> text "u64"
  pretty (SignedValue v) = pretty v
  pretty (BoolValue True) = text "true"
  pretty (BoolValue False) = text "false"
  pretty (FloatValue v) = pretty v

instance IsName vn => Pretty (DimDecl vn) where
  pretty AnyDim = mempty
  pretty (NamedDim v) = pretty v
  pretty (ConstDim n) = pretty n

instance IsName vn => Pretty (DimExp vn) where
  pretty DimExpAny = mempty
  pretty (DimExpNamed v _) = pretty v
  pretty (DimExpConst n _) = pretty n

instance IsName vn => Pretty (ShapeDecl (DimDecl vn)) where
  pretty (ShapeDecl ds) = mconcat (map (brackets . pretty) ds)

instance Pretty (ShapeDecl ()) where
  pretty (ShapeDecl ds) = mconcat $ replicate (length ds) $ text "[]"

instance Pretty (ShapeDecl Int64) where
  pretty (ShapeDecl ds) = mconcat (map (brackets . pretty) ds)

instance Pretty (ShapeDecl Bool) where
  pretty (ShapeDecl ds) = mconcat (map (brackets . pretty) ds)

instance Pretty (ShapeDecl dim) => Pretty (ScalarTypeBase dim as) where
  pretty (Prim et) = pretty et
  pretty (TypeVar _ u et targs) =
    parensIf (not (null targs) && p > 3) $
      pretty u <> pretty (qualNameFromTypeName et) <+> spread (map (pprPrec 3) targs)
  pprPrec _ (Record fs)
    | Just ts <- areTupleFields fs =
      oneLine (parens $ commasep $ map ppr ts)
        <|> parens (align $ mconcat $ punctuate (text "," <> line) $ map ppr ts)
    | otherwise =
      oneLine (braces $ commasep fs')
        <|> braces (align $ mconcat $ punctuate (text "," <> line) fs')
    where
      ppField (name, t) = text (nameToString name) <> colon <+> align (ppr t)
      fs' = map ppField $ M.toList fs
  pprPrec p (Arrow _ (Named v) t1 t2) =
    parensIf (p > 1) $
      parens (pprName v <> colon <+> align (ppr t1)) <+/> text "->" <+> pprPrec 1 t2
  pprPrec p (Arrow _ Unnamed t1 t2) =
    parensIf (p > 1) $ pprPrec 2 t1 <+/> text "->" <+> pprPrec 1 t2
  pprPrec p (Sum cs) =
    parensIf (p > 0) $
      oneLine (mconcat $ punctuate (text " | ") cs')
        <|> align (mconcat $ punctuate (text " |" <> line) cs')
    where
      ppConstr (name, fs) = sep $ (text "#" <> ppr name) : map (pprPrec 1) fs
      cs' = map ppConstr $ M.toList cs

instance Pretty (ShapeDecl dim) => Pretty (TypeBase dim as) where
  pretty (Array _ u at shape) = pretty u <> pretty shape <> align (pretty at)
  pretty (Scalar t) = pretty t

instance Pretty (ShapeDecl dim) => Pretty (TypeArg dim) where
  pretty (TypeArgDim d _) = pretty $ ShapeDecl [d]
  pretty (TypeArgType t _) = pretty t

instance (Eq vn, IsName vn) => Pretty (TypeExp vn) where
  pretty (TEUnique t _) = text "*" <> pretty t
  pretty (TEArray at d _) = brackets (pretty d) <> pretty at
  pretty (TETuple ts _) = parens $ commasep $ map pretty ts
  pretty (TERecord fs _) = braces $ commasep $ map ppField fs
    where
      ppField (name, t) = text (nameToString name) <> colon <+> pretty t
  pretty (TEVar name _) = pretty name
  pretty (TEApply t arg _) = pretty t <+> pretty arg
  pretty (TEArrow (Just v) t1 t2 _) = parens v' <+> text "->" <+> pretty t2
    where
      v' = prettyName v <> colon <+> pretty t1
  pretty (TEArrow Nothing t1 t2 _) = pretty t1 <+> text "->" <+> pretty t2
  pretty (TESum cs _) =
    align $ cat $ punctuate (text " |" <> softline) $ map ppConstr cs
    where
      ppConstr (name, fs) = text "#" <> pretty name <+> sep (map pretty fs)

instance (Eq vn, IsName vn) => Pretty (TypeArgExp vn) where
  pretty (TypeArgExpDim d _) = brackets $ pretty d
  pretty (TypeArgExpType t) = pretty t

instance (Eq vn, IsName vn, Annot f) => Pretty (TypeDeclBase f vn) where
  pretty x = pprAnnot (declaredType x) (expandedType x)

instance IsName vn => Pretty (QualName vn) where
  pretty (QualName names name) =
    mconcat $ punctuate (text ".") $ map pprName names ++ [pprName name]

instance IsName vn => Pretty (IdentBase f vn) where
  pretty = pprName . identName

hasArrayLit :: ExpBase ty vn -> Bool
hasArrayLit ArrayLit {} = True
hasArrayLit (TupLit es2 _) = any hasArrayLit es2
hasArrayLit _ = False

instance (Eq vn, IsName vn, Annot f) => Pretty (DimIndexBase f vn) where
  pretty (DimFix e) = pretty e
  pretty (DimSlice i j (Just s)) =
    maybe mempty pretty i <> text ":"
      <> maybe mempty pretty j
      <> text ":"
      <> pretty s
  pretty (DimSlice i (Just j) s) =
    maybe mempty pretty i <> text ":"
      <> pretty j
      <> maybe mempty ((text ":" <>) . pretty) s
  pretty (DimSlice i Nothing Nothing) =
    maybe mempty pretty i <> text ":"

letBody :: (Eq vn, IsName vn, Annot f) => ExpBase f vn -> Doc
letBody body@LetPat {} = pretty body
letBody body@LetFun {} = pretty body
letBody body = text "in" <+> align (pretty body)

instance (Eq vn, IsName vn, Annot f) => Pretty (ExpBase f vn) where
  ppr = pprPrec (-1)
  pprPrec _ (Var name t _) = ppr name <> inst
    where
      inst = case unAnnot t of
        Just t'
          | isEnvVarSet "FUTHARK_COMPILER_DEBUGGING" False ->
            text "@" <> parens (align $ ppr t')
        _ -> mempty
  pprPrec _ (Parens e _) = align $ parens $ ppr e
  pprPrec _ (QualParens (v, _) e _) = ppr v <> text "." <> align (parens $ ppr e)
  pprPrec p (Ascript e t _) =
    parensIf (p /= -1) $ pprPrec 0 e <+> text ":" <+> align (pprPrec 0 t)
  pprPrec p (Coerce e t _ _) =
    parensIf (p /= -1) $ pprPrec 0 e <+> text ":>" <+> align (pprPrec 0 t)
  pprPrec _ (Literal v _) = ppr v
  pprPrec _ (IntLit v _ _) = ppr v
  pprPrec _ (FloatLit v _ _) = ppr v
  pprPrec _ (TupLit es _)
    | any hasArrayLit es = parens $ commastack $ map ppr es
    | otherwise = parens $ commasep $ map ppr es
  pprPrec _ (RecordLit fs _)
    | any fieldArray fs = braces $ commastack $ map ppr fs
    | otherwise = braces $ commasep $ map ppr fs
    where
      fieldArray (RecordFieldExplicit _ e _) = hasArrayLit e
      fieldArray RecordFieldImplicit {} = False
  pprPrec _ (ArrayLit es info _) =
    brackets (commasep $ map ppr es) <> info'
    where
      info' = case unAnnot info of
        Just t
          | isEnvVarSet "FUTHARK_COMPILER_DEBUGGING" False ->
            text "@" <> parens (align $ ppr t)
        _ -> mempty
  pprPrec _ (StringLit s _) =
    text $ show $ decode s
  pprPrec p (Range start maybe_step end _ _) =
    parensIf (p /= -1) $
      ppr start
        <> maybe mempty ((text ".." <>) . ppr) maybe_step
        <> case end of
          DownToExclusive end' -> text "..>" <> ppr end'
          ToInclusive end' -> text "..." <> ppr end'
          UpToExclusive end' -> text "..<" <> ppr end'
  pprPrec p (BinOp (bop, _) _ (x, _) (y, _) _ _ _) = prettyBinOp p bop x y
  pprPrec _ (Project k e _ _) = ppr e <> text "." <> ppr k
  pprPrec _ (If c t f _ _) =
    text "if" <+> ppr c
      </> text "then" <+> align (ppr t)
      </> text "else" <+> align (ppr f)
  pprPrec p (Apply f arg _ _ _) =
    parensIf (p >= 10) $ pprPrec 0 f <+/> pprPrec 10 arg
  pprPrec _ (Negate e _) = text "-" <> ppr e
  pprPrec p (LetPat pat e body _ _) =
    parensIf (p /= -1) $
      align $
        text "let" <+> align (ppr pat)
          <+> ( if linebreak
                  then equals </> indent 2 (ppr e)
                  else equals <+> align (ppr e)
              )
          </> letBody body
    where
      linebreak = case e of
        DoLoop {} -> True
        LetPat {} -> True
        LetWith {} -> True
        If {} -> True
        Match {} -> True
        Attr {} -> True
        ArrayLit {} -> False
        _ -> hasArrayLit e
  pprPrec _ (LetFun fname (tparams, params, retdecl, rettype, e) body _ _) =
    text "let" <+> pprName fname <+> spread (map ppr tparams ++ map ppr params)
      <> retdecl' <+> equals
      </> indent 2 (ppr e)
      </> letBody body
    where
      retdecl' = case (ppr <$> unAnnot rettype) `mplus` (ppr <$> retdecl) of
        Just rettype' -> colon <+> align rettype'
        Nothing -> mempty
  pprPrec _ (LetWith dest src idxs ve body _ _)
    | dest == src =
      text "let" <+> ppr dest <> list (map ppr idxs)
        <+> equals
        <+> align (ppr ve)
        </> letBody body
    | otherwise =
      text "let" <+> ppr dest <+> equals <+> ppr src
        <+> text "with"
        <+> brackets (commasep (map ppr idxs))
        <+> text "="
        <+> align (ppr ve)
        </> letBody body
  pprPrec _ (Update src idxs ve _) =
    ppr src <+> text "with"
      <+> brackets (commasep (map ppr idxs))
      <+> text "="
      <+> align (ppr ve)
  pprPrec _ (RecordUpdate src fs ve _ _) =
    ppr src <+> text "with"
      <+> mconcat (intersperse (text ".") (map ppr fs))
      <+> text "="
      <+> align (ppr ve)
  pprPrec _ (Index e idxs _ _) =
    pprPrec 9 e <> brackets (commasep (map ppr idxs))
  pprPrec _ (Assert e1 e2 _ _) = text "assert" <+> pprPrec 10 e1 <+> pprPrec 10 e2
  pprPrec p (Lambda params body rettype _ _) =
    parensIf (p /= -1) $
      text "\\" <> spread (map ppr params) <> ppAscription rettype
        <+> text "->" </> indent 2 (ppr body)
  pprPrec _ (OpSection binop _ _) =
    parens $ ppr binop
  pprPrec _ (OpSectionLeft binop _ x _ _ _) =
    parens $ ppr x <+> ppr binop
  pprPrec _ (OpSectionRight binop _ x _ _ _) =
    parens $ ppr binop <+> ppr x
  pprPrec _ (ProjectSection fields _ _) =
    parens $ mconcat $ map p fields
    where
      p name = text "." <> ppr name
  pprPrec _ (IndexSection idxs _ _) =
    parens $ text "." <> brackets (commasep (map ppr idxs))
  pprPrec _ (DoLoop sizeparams pat initexp form loopbody _ _) =
    text "loop"
      <+> align
        ( spread (map (brackets . pprName) sizeparams)
            <+/> ppr pat <+> equals
            <+/> ppr initexp
            <+/> ppr form <+> text "do"
        )
      </> indent 2 (ppr loopbody)
  pprPrec _ (Constr n cs _ _) = text "#" <> ppr n <+> sep (map ppr cs)
  pprPrec _ (Match e cs _ _) = text "match" <+> ppr e </> (stack . map ppr) (NE.toList cs)
  pprPrec _ (Attr attr e _) =
    text "#[" <> ppr attr <> text "]" </> pprPrec (-1) e

instance Pretty AttrInfo where
  ppr (AttrAtom attr) = ppr attr
  ppr (AttrComp f attrs) = ppr f <> parens (commasep $ map ppr attrs)

instance (Eq vn, IsName vn, Annot f) => Pretty (FieldBase f vn) where
  ppr (RecordFieldExplicit name e _) = ppr name <> equals <> ppr e
  ppr (RecordFieldImplicit name _ _) = pprName name

instance (Eq vn, IsName vn, Annot f) => Pretty (CaseBase f vn) where
  ppr (CasePat p e _) = text "case" <+> ppr p <+> text "->" </> indent 2 (ppr e)

instance (Eq vn, IsName vn, Annot f) => Pretty (LoopFormBase f vn) where
  ppr (For i ubound) =
    text "for" <+> ppr i <+> text "<" <+> align (ppr ubound)
  ppr (ForIn x e) =
    text "for" <+> ppr x <+> text "in" <+> ppr e
  ppr (While cond) =
    text "while" <+> ppr cond

instance (Eq vn, IsName vn, Annot f) => Pretty (PatternBase f vn) where
  ppr (PatternAscription p t _) = ppr p <> colon <+> align (ppr t)
  ppr (PatternParens p _) = parens $ ppr p
  ppr (Id v t _) = case unAnnot t of
    Just t' -> parens $ pprName v <> colon <+> align (ppr t')
    Nothing -> pprName v
  ppr (TuplePattern pats _) = parens $ commasep $ map ppr pats
  ppr (RecordPattern fs _) = braces $ commasep $ map ppField fs
    where
      ppField (name, t) = text (nameToString name) <> equals <> ppr t
  ppr (Wildcard t _) = case unAnnot t of
    Just t' -> parens $ text "_" <> colon <+> ppr t'
    Nothing -> text "_"
  ppr (PatternLit e _ _) = ppr e
  ppr (PatternConstr n _ ps _) = text "#" <> ppr n <+> sep (map ppr ps)

ppAscription :: Pretty t => Maybe t -> Doc
ppAscription Nothing = mempty
ppAscription (Just t) = colon <> align (ppr t)

instance (Eq vn, IsName vn, Annot f) => Pretty (ProgBase f vn) where
  ppr = stack . punctuate line . map ppr . progDecs

instance (Eq vn, IsName vn, Annot f) => Pretty (DecBase f vn) where
  ppr (ValDec dec) = ppr dec
  ppr (TypeDec dec) = ppr dec
  ppr (SigDec sig) = ppr sig
  ppr (ModDec sd) = ppr sd
  ppr (OpenDec x _) = text "open" <+> ppr x
  ppr (LocalDec dec _) = text "local" <+> ppr dec
  ppr (ImportDec x _ _) = text "import" <+> ppr x

instance (Eq vn, IsName vn, Annot f) => Pretty (ModExpBase f vn) where
  ppr (ModVar v _) = ppr v
  ppr (ModParens e _) = parens $ ppr e
  ppr (ModImport v _ _) = text "import" <+> ppr (show v)
  ppr (ModDecs ds _) = nestedBlock "{" "}" (stack $ punctuate line $ map ppr ds)
  ppr (ModApply f a _ _ _) = parens $ ppr f <+> parens (ppr a)
  ppr (ModAscript me se _ _) = ppr me <> colon <+> ppr se
  ppr (ModLambda param maybe_sig body _) =
    text "\\" <> ppr param <> maybe_sig'
      <+> text "->" </> indent 2 (ppr body)
    where
      maybe_sig' = case maybe_sig of
        Nothing -> mempty
        Just (sig, _) -> colon <+> ppr sig

instance Pretty Liftedness where
  ppr Unlifted = text ""
  ppr SizeLifted = text "~"
  ppr Lifted = text "^"

instance (Eq vn, IsName vn, Annot f) => Pretty (TypeBindBase f vn) where
  ppr (TypeBind name l params usertype _ _) =
    text "type" <> ppr l <+> pprName name
      <+> spread (map ppr params)
      <+> equals
      <+> ppr usertype

instance (Eq vn, IsName vn) => Pretty (TypeParamBase vn) where
  ppr (TypeParamDim name _) = brackets $ pprName name
  ppr (TypeParamType l name _) = text "'" <> ppr l <> pprName name

instance (Eq vn, IsName vn, Annot f) => Pretty (ValBindBase f vn) where
  ppr (ValBind entry name retdecl rettype tparams args body _ attrs _) =
    mconcat (map ((<> line) . ppr) attrs)
      <> text fun
      <+> pprName name
      <+> align (sep (map ppr tparams ++ map ppr args))
      <> retdecl'
      <> text " ="
      </> indent 2 (ppr body)
    where
      fun
        | isJust entry = "entry"
        | otherwise = "let"
      retdecl' = case (ppr . fst <$> unAnnot rettype) `mplus` (ppr <$> retdecl) of
        Just rettype' -> colon <+> align rettype'
        Nothing -> mempty

instance (Eq vn, IsName vn, Annot f) => Pretty (SpecBase f vn) where
  ppr (TypeAbbrSpec tpsig) = ppr tpsig
  ppr (TypeSpec l name ps _ _) =
    text "type" <> ppr l <+> pprName name <+> spread (map ppr ps)
  ppr (ValSpec name tparams vtype _ _) =
    text "val" <+> pprName name <+> spread (map ppr tparams) <> colon <+> ppr vtype
  ppr (ModSpec name sig _ _) =
    text "module" <+> pprName name <> colon <+> ppr sig
  ppr (IncludeSpec e _) =
    text "include" <+> ppr e

instance (Eq vn, IsName vn, Annot f) => Pretty (SigExpBase f vn) where
  ppr (SigVar v _ _) = ppr v
  ppr (SigParens e _) = parens $ ppr e
  ppr (SigSpecs ss _) = nestedBlock "{" "}" (stack $ punctuate line $ map ppr ss)
  ppr (SigWith s (TypeRef v ps td _) _) =
    ppr s <+> text "with" <+> ppr v <+> spread (map ppr ps) <> text " =" <+> ppr td
  ppr (SigArrow (Just v) e1 e2 _) =
    parens (pprName v <> colon <+> ppr e1) <+> text "->" <+> ppr e2
  ppr (SigArrow Nothing e1 e2 _) =
    ppr e1 <+> text "->" <+> ppr e2

instance (Eq vn, IsName vn, Annot f) => Pretty (SigBindBase f vn) where
  ppr (SigBind name e _ _) =
    text "module type" <+> pprName name <+> equals <+> ppr e

instance (Eq vn, IsName vn, Annot f) => Pretty (ModParamBase f vn) where
  ppr (ModParam pname psig _ _) =
    parens (pprName pname <> colon <+> ppr psig)

instance (Eq vn, IsName vn, Annot f) => Pretty (ModBindBase f vn) where
  ppr (ModBind name ps sig e _ _) =
    text "module" <+> pprName name <+> spread (map ppr ps) <+> sig' <> text " =" <+> ppr e
    where
      sig' = case sig of
        Nothing -> mempty
        Just (s, _) -> colon <+> ppr s <> text " "

prettyBinOp ::
  (Eq vn, IsName vn, Annot f) =>
  Int ->
  QualName vn ->
  ExpBase f vn ->
  ExpBase f vn ->
  Doc
prettyBinOp p bop x y =
  parensIf (p > symPrecedence) $
    pprPrec symPrecedence x
      <+/> bop'
      <+> pprPrec symRPrecedence y
  where
    bop' = case leading of
      Backtick -> text "`" <> ppr bop <> text "`"
      _ -> ppr bop
    leading = leadingOperator $ nameFromString $ pretty $ pprName $ qualLeaf bop
    symPrecedence = precedence leading
    symRPrecedence = rprecedence leading
    precedence PipeRight = -1
    precedence PipeLeft = -1
    precedence LogAnd = 0
    precedence LogOr = 0
    precedence Band = 1
    precedence Bor = 1
    precedence Xor = 1
    precedence Equal = 2
    precedence NotEqual = 2
    precedence Less = 2
    precedence Leq = 2
    precedence Greater = 2
    precedence Geq = 2
    precedence ShiftL = 3
    precedence ShiftR = 3
    precedence Plus = 4
    precedence Minus = 4
    precedence Times = 5
    precedence Divide = 5
    precedence Mod = 5
    precedence Quot = 5
    precedence Rem = 5
    precedence Pow = 6
    precedence Backtick = 9
    rprecedence Minus = 10
    rprecedence Divide = 10
    rprecedence op = precedence op
