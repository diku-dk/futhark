{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Representation.AST.Lore
       ( Lore(..)
       )
       where

import Futhark.Representation.AST.RetType
import Futhark.Representation.AST.Syntax.Core

class (Show (LetBound l), Show (Exp l), Show (Body l), Show (FParam l), Show (RetType l),
       Eq (LetBound l), Eq (Exp l), Eq (Body l), Eq (FParam l), Eq (RetType l),
       Ord (LetBound l), Ord (Exp l), Ord (Body l), Ord (FParam l), Ord (RetType l),
       IsRetType (RetType l))
      => Lore l where
  -- | Annotation for every binding.
  type LetBound l :: *
  type LetBound l = ()
  -- | Annotation for every expression.
  type Exp l :: *
  type Exp l = ()
  -- | Annotation for every body.
  type Body l :: *
  type Body l = ()
  -- | Annotation for every (non-lambda) function parameter.
  type FParam l :: *
  type FParam l = ()
  -- | The type of expressions and function calls.
  type RetType l :: *
  type RetType l = ExtRetType

  -- | A constant used to disambiguate method calls.  XXX, this is a
  -- hack to get around mising type application in Haskell, sometimes
  -- resulting in ambiguous types.
  representative :: l

  -- | A loop returns not only the values indicated in the result list
  -- @res@, but may also have an existential context.  Thus,
  -- @loopResult res merge@ returns those variables in @merge@ that
  -- constitute the context.
  loopResultContext :: l
                    -> [Ident]
                    -> [Futhark.Representation.AST.Syntax.Core.FParamT (FParam l)]
                    -> [Ident]

  -- | Given a function return type, the parameters of the function,
  -- and the arguments for a concrete call, return the instantiated
  -- return type for the concrete call, if valid.
  applyRetType :: l
               -> RetType l
               -> [Futhark.Representation.AST.Syntax.Core.FParamT (FParam l)]
               -> [SubExp]
               -> Maybe (RetType l)
