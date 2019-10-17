{-# LANGUAGE TypeFamilies, FlexibleContexts, FlexibleInstances, ConstraintKinds #-}
-- | This module provides various simple ways to query and manipulate
-- fundamental Futhark terms, such as types and values.  The intent is to
-- keep "Futhark.Reprsentation.AST.Syntax" simple, and put whatever
-- embellishments we need here.  This is an internal, desugared
-- representation.
module Futhark.Representation.AST.Attributes
  ( module Futhark.Representation.AST.Attributes.Reshape
  , module Futhark.Representation.AST.Attributes.Rearrange
  , module Futhark.Representation.AST.Attributes.Types
  , module Futhark.Representation.AST.Attributes.Constants
  , module Futhark.Representation.AST.Attributes.TypeOf
  , module Futhark.Representation.AST.Attributes.Patterns
  , module Futhark.Representation.AST.Attributes.Names
  , module Futhark.Representation.AST.RetType

  -- * Built-in functions
  , isBuiltInFunction
  , builtInFunctions

  -- * Extra tools
  , funDefByName
  , asBasicOp
  , safeExp
  , subExpVars
  , subExpVar
  , shapeVars
  , commutativeLambda
  , entryPointSize
  , defAux
  , stmCerts
  , certify
  , expExtTypesFromPattern
  , patternFromParams

  , IsOp (..)
  , Attributes (..)
  )
  where

import Data.List
import Data.Maybe (mapMaybe, isJust)
import qualified Data.Map.Strict as M

import Futhark.Representation.AST.Attributes.Reshape
import Futhark.Representation.AST.Attributes.Rearrange
import Futhark.Representation.AST.Attributes.Types
import Futhark.Representation.AST.Attributes.Constants
import Futhark.Representation.AST.Attributes.Patterns
import Futhark.Representation.AST.Attributes.Names
import Futhark.Representation.AST.Attributes.TypeOf
import Futhark.Representation.AST.RetType
import Futhark.Representation.AST.Syntax
import Futhark.Representation.AST.Pretty
import Futhark.Transform.Rename (Rename, Renameable)
import Futhark.Transform.Substitute (Substitute, Substitutable)
import Futhark.Util.Pretty

-- | @isBuiltInFunction k@ is 'True' if @k@ is an element of 'builtInFunctions'.
isBuiltInFunction :: Name -> Bool
isBuiltInFunction fnm = fnm `M.member` builtInFunctions

-- | A map of all built-in functions and their types.
builtInFunctions :: M.Map Name (PrimType,[PrimType])
builtInFunctions = M.fromList $ map namify $ M.toList primFuns
  where namify (k,(paramts,ret,_)) = (nameFromString k, (ret, paramts))

-- | Find the function of the given name in the Futhark program.
funDefByName :: Name -> Prog lore -> Maybe (FunDef lore)
funDefByName fname = find ((fname ==) . funDefName) . progFunctions

-- | If the expression is a 'BasicOp', return that 'BasicOp', otherwise 'Nothing'.
asBasicOp :: Exp lore -> Maybe (BasicOp lore)
asBasicOp (BasicOp op) = Just op
asBasicOp _           = Nothing

-- | An expression is safe if it is always well-defined (assuming that
-- any required certificates have been checked) in any context.  For
-- example, array indexing is not safe, as the index may be out of
-- bounds.  On the other hand, adding two numbers cannot fail.
safeExp :: IsOp (Op lore) => Exp lore -> Bool
safeExp (BasicOp op) = safeBasicOp op
  where safeBasicOp (BinOp SDiv{} _ (Constant y)) = not $ zeroIsh y
        safeBasicOp (BinOp SDiv{} _ _) = False
        safeBasicOp (BinOp UDiv{} _ (Constant y)) = not $ zeroIsh y
        safeBasicOp (BinOp UDiv{} _ _) = False
        safeBasicOp (BinOp SMod{} _ (Constant y)) = not $ zeroIsh y
        safeBasicOp (BinOp SMod{} _ _) = False
        safeBasicOp (BinOp UMod{} _ (Constant y)) = not $ zeroIsh y
        safeBasicOp (BinOp UMod{} _ _) = False

        safeBasicOp (BinOp SQuot{} _ (Constant y)) = not $ zeroIsh y
        safeBasicOp (BinOp SQuot{} _ _) = False
        safeBasicOp (BinOp SRem{} _ (Constant y)) = not $ zeroIsh y
        safeBasicOp (BinOp SRem{} _ _) = False

        safeBasicOp (BinOp Pow{} _ (Constant y)) = not $ negativeIsh y
        safeBasicOp (BinOp Pow{} _ _) = False
        safeBasicOp ArrayLit{} = True
        safeBasicOp BinOp{} = True
        safeBasicOp SubExp{} = True
        safeBasicOp UnOp{} = True
        safeBasicOp CmpOp{} = True
        safeBasicOp ConvOp{} = True
        safeBasicOp Scratch{} = True
        safeBasicOp Concat{} = True
        safeBasicOp Reshape{} = True
        safeBasicOp Rearrange{} = True
        safeBasicOp Manifest{} = True
        safeBasicOp Iota{} = True
        safeBasicOp Replicate{} = True
        safeBasicOp Copy{} = True
        safeBasicOp _ = False

safeExp (DoLoop _ _ _ body) = safeBody body
safeExp (Apply fname _ _ _) = isBuiltInFunction fname
safeExp (If _ tbranch fbranch _) =
  all (safeExp . stmExp) (bodyStms tbranch) &&
  all (safeExp . stmExp) (bodyStms fbranch)
safeExp (Op op) = safeOp op

safeBody :: IsOp (Op lore) => Body lore -> Bool
safeBody = all (safeExp . stmExp) . bodyStms

-- | Return the variable names used in 'Var' subexpressions.  May contain
-- duplicates.
subExpVars :: [SubExp] -> [VName]
subExpVars = mapMaybe subExpVar

-- | If the 'SubExp' is a 'Var' return the variable name.
subExpVar :: SubExp -> Maybe VName
subExpVar (Var v)    = Just v
subExpVar Constant{} = Nothing

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
      (xps,yps) = splitAt n2 (lambdaParams lam)

      okComponent c = isJust $ find (okBinOp c) $ bodyStms body
      okBinOp (xp,yp,Var r) (Let (Pattern [] [pe]) _ (BasicOp (BinOp op (Var x) (Var y)))) =
        patElemName pe == r &&
        commutativeBinOp op &&
        ((x == paramName xp && y == paramName yp) ||
         (y == paramName xp && x == paramName yp))
      okBinOp _ _ = False

  in n2 * 2 == length (lambdaParams lam) &&
     n2 == length (bodyResult body) &&
     all okComponent (zip3 xps yps $ bodyResult body)

-- | How many value parameters are accepted by this entry point?  This
-- is used to determine which of the function parameters correspond to
-- the parameters of the original function (they must all come at the
-- end).
entryPointSize :: EntryPointType -> Int
entryPointSize (TypeOpaque _ x) = x
entryPointSize TypeUnsigned = 1
entryPointSize TypeDirect = 1

-- | A 'StmAux' with empty 'Certificates'.
defAux :: attr -> StmAux attr
defAux = StmAux mempty

-- | The certificates associated with a statement.
stmCerts :: Stm lore -> Certificates
stmCerts = stmAuxCerts . stmAux

-- | Add certificates to a statement.
certify :: Certificates -> Stm lore -> Stm lore
certify cs1 (Let pat (StmAux cs2 attr) e) = Let pat (StmAux (cs2<>cs1) attr) e

-- | A type class for operations.
class (Eq op, Ord op, Show op,
       TypedOp op,
       Rename op,
       Substitute op,
       FreeIn op,
       Pretty op) => IsOp op where
  -- | Like 'safeExp', but for arbitrary ops.
  safeOp :: op -> Bool
  -- | Should we try to hoist this out of branches?
  cheapOp :: op -> Bool

instance IsOp () where
  safeOp () = True
  cheapOp () = True

-- | Lore-specific attributes; also means the lore supports some basic
-- facilities.
class (Annotations lore,

       PrettyLore lore,

       Renameable lore, Substitutable lore,
       FreeAttr (ExpAttr lore),
       FreeIn (LetAttr lore),
       FreeAttr (BodyAttr lore),
       FreeIn (FParamAttr lore),
       FreeIn (LParamAttr lore),
       FreeIn (RetType lore),
       FreeIn (BranchType lore),

       IsOp (Op lore)) => Attributes lore where
  -- | Given a pattern, construct the type of a body that would match
  -- it.  An implementation for many lores would be
  -- 'expExtTypesFromPattern'.
  expTypesFromPattern :: (HasScope lore m, Monad m) =>
                         Pattern lore -> m [BranchType lore]

-- | Construct the type of an expression that would match the pattern.
expExtTypesFromPattern :: Typed attr => PatternT attr -> [ExtType]
expExtTypesFromPattern pat =
  existentialiseExtTypes (patternContextNames pat) $
  staticShapes $ map patElemType $ patternValueElements pat

-- | Create a pattern corresponding to some parameters.
patternFromParams :: [Param attr] -> PatternT attr
patternFromParams = Pattern [] . map toPatElem
  where toPatElem p = PatElem (paramName p) $ paramAttr p
