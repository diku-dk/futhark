{-# LANGUAGE TypeFamilies #-}

-- | This module provides various simple ways to query and manipulate
-- fundamental Futhark terms, such as types and values.  The intent is
-- to keep "Futhark.IR.Syntax" simple, and put whatever embellishments
-- we need here.  This is an internal, desugared representation.
module Futhark.IR.Prop
  ( module Futhark.IR.Prop.Reshape,
    module Futhark.IR.Prop.Rearrange,
    module Futhark.IR.Prop.Types,
    module Futhark.IR.Prop.Constants,
    module Futhark.IR.Prop.TypeOf,
    module Futhark.IR.Prop.Pat,
    module Futhark.IR.Prop.Names,
    module Futhark.IR.RetType,
    module Futhark.IR.Rephrase,

    -- * Built-in functions
    isBuiltInFunction,
    builtInFunctions,

    -- * Extra tools
    asBasicOp,
    safeExp,
    subExpVars,
    subExpVar,
    commutativeLambda,
    defAux,
    stmCerts,
    certify,
    expExtTypesFromPat,
    attrsForAssert,
    lamIsBinOp,
    ASTConstraints,
    IsOp (..),
    ASTRep (..),
  )
where

import Control.Monad
import Data.List (elemIndex, find)
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, mapMaybe)
import Data.Set qualified as S
import Futhark.IR.Pretty
import Futhark.IR.Prop.Constants
import Futhark.IR.Prop.Names
import Futhark.IR.Prop.Pat
import Futhark.IR.Prop.Rearrange
import Futhark.IR.Prop.Reshape
import Futhark.IR.Prop.TypeOf
import Futhark.IR.Prop.Types
import Futhark.IR.Rephrase
import Futhark.IR.RetType
import Futhark.IR.Syntax
import Futhark.Transform.Rename (Rename, Renameable)
import Futhark.Transform.Substitute (Substitutable, Substitute)
import Futhark.Util (maybeNth)

-- | @isBuiltInFunction k@ is 'True' if @k@ is an element of 'builtInFunctions'.
isBuiltInFunction :: Name -> Bool
isBuiltInFunction fnm = fnm `M.member` builtInFunctions

-- | A map of all built-in functions and their types.
builtInFunctions :: M.Map Name (PrimType, [PrimType])
builtInFunctions = M.fromList $ map namify $ M.toList primFuns
  where
    namify (k, (paramts, ret, _)) = (nameFromText k, (ret, paramts))

-- | If the expression is a t'BasicOp', return it, otherwise 'Nothing'.
asBasicOp :: Exp rep -> Maybe BasicOp
asBasicOp (BasicOp op) = Just op
asBasicOp _ = Nothing

-- | An expression is safe if it is always well-defined (assuming that
-- any required certificates have been checked) in any context.  For
-- example, array indexing is not safe, as the index may be out of
-- bounds.  On the other hand, adding two numbers cannot fail.
safeExp :: (ASTRep rep) => Exp rep -> Bool
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
    safeBasicOp _ = False
safeExp (Loop _ _ body) = safeBody body
safeExp (Apply fname _ _ _) =
  isBuiltInFunction fname
safeExp (Match _ cases def_case _) =
  all (all (safeExp . stmExp) . bodyStms . caseBody) cases
    && all (safeExp . stmExp) (bodyStms def_case)
safeExp WithAcc {} = True -- Although unlikely to matter.
safeExp (Op op) = safeOp op

safeBody :: (ASTRep rep) => Body rep -> Bool
safeBody = all (safeExp . stmExp) . bodyStms

-- | Return the variable names used in 'Var' subexpressions.  May contain
-- duplicates.
subExpVars :: [SubExp] -> [VName]
subExpVars = mapMaybe subExpVar

-- | If the t'SubExp' is a 'Var' return the variable name.
subExpVar :: SubExp -> Maybe VName
subExpVar (Var v) = Just v
subExpVar Constant {} = Nothing

-- | Does the given lambda represent a known commutative function?
-- Based on pattern matching and checking whether the lambda
-- represents a known arithmetic operator; don't expect anything
-- clever here.
commutativeLambda :: Lambda rep -> Bool
commutativeLambda lam =
  let body = lambdaBody lam
      n2 = length (lambdaParams lam) `div` 2
      (xps, yps) = splitAt n2 (lambdaParams lam)

      okComponent c = isJust $ find (okBinOp c) $ bodyStms body
      okBinOp
        (xp, yp, SubExpRes _ (Var r))
        (Let (Pat [pe]) _ (BasicOp (BinOp op (Var x) (Var y)))) =
          patElemName pe == r
            && commutativeBinOp op
            && ( (x == paramName xp && y == paramName yp)
                   || (y == paramName xp && x == paramName yp)
               )
      okBinOp _ _ = False
   in n2 * 2 == length (lambdaParams lam)
        && n2 == length (bodyResult body)
        && all okComponent (zip3 xps yps $ bodyResult body)

-- | A 'StmAux' with empty 'Certs'.
defAux :: dec -> StmAux dec
defAux = StmAux mempty mempty mempty

-- | The certificates associated with a statement.
stmCerts :: Stm rep -> Certs
stmCerts = stmAuxCerts . stmAux

-- | Add certificates to a statement.
certify :: Certs -> Stm rep -> Stm rep
certify cs1 (Let pat aux e) =
  Let pat (aux {stmAuxCerts = stmAuxCerts aux <> cs1}) e

-- | A handy shorthand for properties that we usually want for things
-- we stuff into ASTs.
type ASTConstraints a =
  (Eq a, Ord a, Show a, Rename a, Substitute a, FreeIn a, Pretty a)

-- | A type class for operations.
class (TypedOp op) => IsOp op where
  -- | Like 'safeExp', but for arbitrary ops.
  safeOp :: (ASTRep rep) => op rep -> Bool

  -- | Should we try to hoist this out of branches?
  cheapOp :: (ASTRep rep) => op rep -> Bool

  -- | Compute the data dependencies of an operation.
  opDependencies :: (ASTRep rep) => op rep -> [Names]

instance IsOp NoOp where
  safeOp NoOp = True
  cheapOp NoOp = True
  opDependencies NoOp = []

-- | Representation-specific attributes; also means the rep supports
-- some basic facilities.
class
  ( RepTypes rep,
    PrettyRep rep,
    Renameable rep,
    Substitutable rep,
    FreeDec (ExpDec rep),
    FreeIn (LetDec rep),
    FreeDec (BodyDec rep),
    FreeIn (FParamInfo rep),
    FreeIn (LParamInfo rep),
    FreeIn (RetType rep),
    FreeIn (BranchType rep),
    ASTConstraints (OpC rep rep),
    IsOp (OpC rep),
    RephraseOp (OpC rep)
  ) =>
  ASTRep rep
  where
  -- | Given a pattern, construct the type of a body that would match
  -- it.  An implementation for many representations would be
  -- 'expExtTypesFromPat'.
  expTypesFromPat ::
    (HasScope rep m, Monad m) =>
    Pat (LetDec rep) ->
    m [BranchType rep]

-- | Construct the type of an expression that would match the pattern.
expExtTypesFromPat :: (Typed dec) => Pat dec -> [ExtType]
expExtTypesFromPat pat =
  existentialiseExtTypes (patNames pat) $
    staticShapes $
      map patElemType $
        patElems pat

-- | Keep only those attributes that are relevant for 'Assert'
-- expressions.
attrsForAssert :: Attrs -> Attrs
attrsForAssert (Attrs attrs) =
  Attrs $ S.filter attrForAssert attrs
  where
    attrForAssert = (== AttrComp "warn" ["safety_checks"])

-- | Horizontally fission a lambda that models a binary operator.
lamIsBinOp :: (ASTRep rep) => Lambda rep -> Maybe [(BinOp, PrimType, VName, VName)]
lamIsBinOp lam = mapM splitStm $ bodyResult $ lambdaBody lam
  where
    n = length $ lambdaReturnType lam
    splitStm (SubExpRes cs (Var res)) = do
      guard $ cs == mempty
      Let (Pat [pe]) _ (BasicOp (BinOp op (Var x) (Var y))) <-
        find (([res] ==) . patNames . stmPat) $
          stmsToList $
            bodyStms $
              lambdaBody lam
      i <- Var res `elemIndex` map resSubExp (bodyResult (lambdaBody lam))
      xp <- maybeNth i $ lambdaParams lam
      yp <- maybeNth (n + i) $ lambdaParams lam
      guard $ paramName xp == x
      guard $ paramName yp == y
      Prim t <- Just $ patElemType pe
      pure (op, t, paramName xp, paramName yp)
    splitStm _ = Nothing
