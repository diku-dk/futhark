{-# LANGUAGE FlexibleContexts #-}
-- | Futhark error definitions.
module Futhark.Error
  ( CompilerError(..)
  , ErrorClass(..)

  , externalError
  , externalErrorS

  , InternalError
  , internalError
  , compilerBug
  , compilerBugS
  , compilerLimitation
  , compilerLimitationS
  )
where

import Control.Monad.Error.Class
import qualified Data.Text as T
import Futhark.Util.Pretty

-- | There are two classes of internal errors: actual bugs, and
-- implementation limitations.  The latter are already known and need
-- not be reported.
data ErrorClass = CompilerBug
                | CompilerLimitation
                deriving (Eq, Ord, Show)

data CompilerError =
    ExternalError Doc
    -- ^ An error that happened due to something the user did, such as
    -- provide incorrect code or options.
  | InternalError T.Text T.Text ErrorClass
    -- ^ An internal compiler error.  The second text is extra data
    -- for debugging, which can be written to a file.

instance Show CompilerError where
  show (ExternalError s) = pretty s
  show (InternalError s _ _) = T.unpack s

externalError :: MonadError CompilerError m => Doc -> m a
externalError = throwError . ExternalError

externalErrorS :: MonadError CompilerError m => String -> m a
externalErrorS = externalError . text

-- | An error that is not the users fault, but a bug (or limitation)
-- in the compiler.  Compiler passes should only ever report this
-- error - any problems after the type checker are *our* fault, not
-- the users.
data InternalError = Error ErrorClass T.Text

compilerBug :: MonadError InternalError m => T.Text -> m a
compilerBug = throwError . Error CompilerBug

compilerLimitation :: MonadError InternalError m => T.Text -> m a
compilerLimitation = throwError . Error CompilerLimitation

internalError :: MonadError CompilerError m => InternalError -> T.Text -> m a
internalError (Error c s) t = throwError $ InternalError s t c

compilerBugS :: MonadError InternalError m => String -> m a
compilerBugS = compilerBug . T.pack

compilerLimitationS :: MonadError InternalError m => String -> m a
compilerLimitationS = compilerLimitation . T.pack
