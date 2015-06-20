{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Futhark.Representation.AST.Lore
       ( Lore(..)
       )
       where

import Futhark.Representation.AST.Syntax
import qualified Futhark.Representation.AST.Annotations as Annotations
import Futhark.Representation.AST.Attributes.TypeEnv
import Futhark.Representation.AST.Attributes.Context

class Annotations.Annotations lore => Lore lore where
  -- | A constant used to disambiguate method calls.  XXX, this is a
  -- hack to get around mising type application in Haskell, sometimes
  -- resulting in ambiguous types.
  representative :: lore

  -- | A loop returns not only the values indicated in the result list
  -- @res@, but may also have an existential context.  Thus,
  -- @loopResult res merge@ returns those variables in @merge@ that
  -- constitute the context.
  loopResultContext :: lore
                    -> [VName]
                    -> [FParam lore]
                    -> [VName]

  -- | As far as possible, determine the subexpression to which ecah
  -- context pattern element will be bound due to evaluation of the
  -- given expression.  The resulting list must have the same number
  -- of elements as there are context elements in the pattern.
  --
  -- The default method invokes 'expExtContext'.
  expContext :: (HasTypeEnv m, Monad m) =>
                Pattern lore ->
                Exp lore ->
                m [Maybe SubExp]
  expContext = expExtContext

  -- | Given a function return type, the parameters of the function,
  -- and the arguments for a concrete call, return the instantiated
  -- return type for the concrete call, if valid.
  applyRetType :: lore
               -> RetType lore
               -> [FParam lore]
               -> [(SubExp, Type)]
               -> Maybe (RetType lore)
