{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides various simple ways to query and manipulate
-- fundamental Futhark terms, such as types and values.  The intent is to
-- keep "Futhark.IRrsentation.AST.Syntax" simple, and put whatever
-- embellishments we need here.  This is an internal, desugared
-- representation.
module Futhark.IR.Prop
  ( module Futhark.IR.Prop.Reshape,
    module Futhark.IR.Prop.Rearrange,
    module Futhark.IR.Prop.Types,
    module Futhark.IR.Prop.Constants,
    module Futhark.IR.Prop.TypeOf,
    module Futhark.IR.Prop.Patterns,
    module Futhark.IR.Prop.Names,
    module Futhark.IR.RetType,

    -- * Built-in functions
    isBuiltInFunction,
    builtInFunctions,

    -- * Extra tools
    asBasicOp,
    safeExp,
    subExpVars,
    subExpVar,
    shapeVars,
    commutativeLambda,
    entryPointSize,
    defAux,
    stmCerts,
    certify,
    expExtTypesFromPattern,
    attrsForAssert,
    ASTConstraints,
    IsOp (..),
    ASTLore (..),
  )
where

import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as S
import Futhark.IR.Pretty
import Futhark.IR.Prop.Constants
import Futhark.IR.Prop.Names
import Futhark.IR.Prop.Patterns
import Futhark.IR.Prop.Rearrange
import Futhark.IR.Prop.Reshape
import Futhark.IR.Prop.TypeOf
import Futhark.IR.Prop.Types
import Futhark.IR.RetType
import Futhark.IR.Syntax
import Futhark.Transform.Rename (Rename, Renameable)
import Futhark.Transform.Substitute (Substitutable, Substitute)
import Futhark.Util.Pretty
import Language.SexpGrammar as Sexp

-- | @isBuiltInFunction k@ is 'True' if @k@ is an element of 'builtInFunctions'.
isBuiltInFunction :: Name -> Bool
isBuiltInFunction fnm = fnm `M.member` builtInFunctions

-- | A map of all built-in functions and their types.
builtInFunctions :: M.Map Name (PrimType, [PrimType])
builtInFunctions = M.fromList $ map namify $ M.toList primFuns
  where
    namify (k, (paramts, ret, _)) = (nameFromString k, (ret, paramts))

-- | If the expression is a t'BasicOp', return it, otherwise 'Nothing'.
asBasicOp :: Exp lore -> Maybe BasicOp
asBasicOp (BasicOp op) = Just op
asBasicOp _ = Nothing

-- | An expression is safe if it is always well-defined (assuming that
-- any required certificates have been checked) in any context.  For
-- example, array indexing is not safe, as the index may be out of
-- bounds.  On the other hand, adding two numbers cannot fail.
safeExp :: IsOp (Op lore) => Exp lore -> Bool
safeExp (BasicOp op) = safeBasicOp op
  where
    safeBasicOp (BinOp (SDiv _ Safe) _ _) = True
    safeBasicOp (BinOp (SDivUp _ Safe) _ _) = True
    safeBasicOp (BinOp (SQuot _ Safe) _ _) = True
    safeBasicOp (BinOp (UDiv _ Safe) _ _) = True
    safeBasicOp (BinOp (UDivUp _ Safe) _ _) = True
    safeBasicOp (BinOp (SMod _ Safe) _ _) = True
    safeBasicOp (BinOp (SRem _ Safe) _ _) = True
    safeBasicOp (BinOp (UMod _ Safe) _ _) = True
    safeBasicOp (BinOp SDiv {} _ (Constant y)) = not $ zeroIsh y
    safeBasicOp (BinOp SDiv {} _ _) = False
    safeBasicOp (BinOp SDivUp {} _ (Constant y)) = not $ zeroIsh y
    safeBasicOp (BinOp SDivUp {} _ _) = False
    safeBasicOp (BinOp UDiv {} _ (Constant y)) = not $ zeroIsh y
    safeBasicOp (BinOp UDiv {} _ _) = False
    safeBasicOp (BinOp UDivUp {} _ (Constant y)) = not $ zeroIsh y
    safeBasicOp (BinOp UDivUp {} _ _) = False
    safeBasicOp (BinOp SMod {} _ (Constant y)) = not $ zeroIsh y
    safeBasicOp (BinOp SMod {} _ _) = False
    safeBasicOp (BinOp UMod {} _ (Constant y)) = not $ zeroIsh y
    safeBasicOp (BinOp UMod {} _ _) = False
    safeBasicOp (BinOp SQuot {} _ (Constant y)) = not $ zeroIsh y
    safeBasicOp (BinOp SQuot {} _ _) = False
    safeBasicOp (BinOp SRem {} _ (Constant y)) = not $ zeroIsh y
    safeBasicOp (BinOp SRem {} _ _) = False
    safeBasicOp (BinOp Pow {} _ (Constant y)) = not $ negativeIsh y
    safeBasicOp (BinOp Pow {} _ _) = False
    safeBasicOp ArrayLit {} = True
    safeBasicOp BinOp {} = True
    safeBasicOp SubExp {} = True
    safeBasicOp UnOp {} = True
    safeBasicOp CmpOp {} = True
    safeBasicOp ConvOp {} = True
    safeBasicOp Scratch {} = True
    safeBasicOp Concat {} = True
    safeBasicOp Reshape {} = True
    safeBasicOp Rearrange {} = True
    safeBasicOp Manifest {} = True
    safeBasicOp Iota {} = True
    safeBasicOp Replicate {} = True
    safeBasicOp Copy {} = True
    safeBasicOp _ = False
safeExp (DoLoop _ _ _ body) = safeBody body
safeExp (Apply fname _ _ _) =
  isBuiltInFunction fname
safeExp (If _ tbranch fbranch _) =
  all (safeExp . stmExp) (bodyStms tbranch)
    && all (safeExp . stmExp) (bodyStms fbranch)
safeExp (Op op) = safeOp op

safeBody :: IsOp (Op lore) => Body lore -> Bool
safeBody = all (safeExp . stmExp) . bodyStms

-- | Return the variable names used in 'Var' subexpressions.  May contain
-- duplicates.
subExpVars :: [SubExp] -> [VName]
subExpVars = mapMaybe subExpVar

-- | If the t'SubExp' is a 'Var' return the variable name.
subExpVar :: SubExp -> Maybe VName
subExpVar (Var v) = Just v
subExpVar Constant {} = Nothing

-- | Return the variable dimension sizes.  May contain
-- duplicates.
shapeVars :: Shape -> [VName]
shapeVars = subExpVars . shapeDims

-- | Does the given lambda represent a known commutative function?
-- Based on pattern matching and checking whether the lambda
-- represents a known arithmetic operator; don't expect anything
-- clever here.
commutativeLambda :: Lambda lore -> Bool
commutativeLambda lam =
  let body = lambdaBody lam
      n2 = length (lambdaParams lam) `div` 2
      (xps, yps) = splitAt n2 (lambdaParams lam)

      okComponent c = isJust $ find (okBinOp c) $ bodyStms body
      okBinOp (xp, yp, Var r) (Let (Pattern [] [pe]) _ (BasicOp (BinOp op (Var x) (Var y)))) =
        patElemName pe == r
          && commutativeBinOp op
          && ( (x == paramName xp && y == paramName yp)
                 || (y == paramName xp && x == paramName yp)
             )
      okBinOp _ _ = False
   in n2 * 2 == length (lambdaParams lam)
        && n2 == length (bodyResult body)
        && all okComponent (zip3 xps yps $ bodyResult body)

-- | How many value parameters are accepted by this entry point?  This
-- is used to determine which of the function parameters correspond to
-- the parameters of the original function (they must all come at the
-- end).
entryPointSize :: EntryPointType -> Int
entryPointSize (TypeOpaque _ x) = x
entryPointSize TypeUnsigned = 1
entryPointSize TypeDirect = 1

-- | A 'StmAux' with empty 'Certificates'.
defAux :: dec -> StmAux dec
defAux = StmAux mempty mempty

-- | The certificates associated with a statement.
stmCerts :: Stm lore -> Certificates
stmCerts = stmAuxCerts . stmAux

-- | Add certificates to a statement.
certify :: Certificates -> Stm lore -> Stm lore
certify cs1 (Let pat (StmAux cs2 attrs dec) e) =
  Let pat (StmAux (cs2 <> cs1) attrs dec) e

-- | A handy shorthand for properties that we usually want to things
-- we stuff into ASTs.
type ASTConstraints a =
  (Eq a, Ord a, Show a, Rename a, Substitute a, FreeIn a, Pretty a, SexpIso a)

-- | A type class for operations.
class (ASTConstraints op, TypedOp op) => IsOp op where
  -- | Like 'safeExp', but for arbitrary ops.
  safeOp :: op -> Bool

  -- | Should we try to hoist this out of branches?
  cheapOp :: op -> Bool

instance IsOp () where
  safeOp () = True
  cheapOp () = True

-- | Lore-specific attributes; also means the lore supports some basic
-- facilities.
class
  ( Decorations lore,
    PrettyLore lore,
    Renameable lore,
    Substitutable lore,
    FreeDec (ExpDec lore),
    FreeIn (LetDec lore),
    FreeDec (BodyDec lore),
    FreeIn (FParamInfo lore),
    FreeIn (LParamInfo lore),
    FreeIn (RetType lore),
    FreeIn (BranchType lore),
    IsOp (Op lore)
  ) =>
  ASTLore lore
  where
  -- | Given a pattern, construct the type of a body that would match
  -- it.  An implementation for many lores would be
  -- 'expExtTypesFromPattern'.
  expTypesFromPattern ::
    (HasScope lore m, Monad m) =>
    Pattern lore ->
    m [BranchType lore]

-- | Construct the type of an expression that would match the pattern.
expExtTypesFromPattern :: Typed dec => PatternT dec -> [ExtType]
expExtTypesFromPattern pat =
  existentialiseExtTypes (patternContextNames pat) $
    staticShapes $ map patElemType $ patternValueElements pat

-- | Keep only those attributes that are relevant for 'Assert'
-- expressions.
attrsForAssert :: Attrs -> Attrs
attrsForAssert (Attrs attrs) =
  Attrs $ S.filter attrForAssert attrs
  where
    attrForAssert = (== AttrComp "warn" ["safety_checks"])
