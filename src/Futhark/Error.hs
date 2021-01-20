{-# LANGUAGE FlexibleContexts #-}

-- | Futhark error definitions.
module Futhark.Error
  ( CompilerError (..),
    ErrorClass (..),
    externalError,
    externalErrorS,
    InternalError (..),
    compilerBug,
    compilerBugS,
    compilerLimitation,
    compilerLimitationS,
    internalErrorS,
  )
where

import Control.Exception
import Control.Monad.Error.Class
import qualified Data.Text as T
import Futhark.Util.Pretty

-- | There are two classes of internal errors: actual bugs, and
-- implementation limitations.  The latter are already known and need
-- not be reported.
data ErrorClass
  = CompilerBug
  | CompilerLimitation
  deriving (Eq, Ord, Show)

-- | A compiler error.
data CompilerError
  = -- | An error that happened due to something the user did, such as
    -- provide incorrect code or options.
    ExternalError Doc
  | -- | An internal compiler error.  The second text is extra data
    -- for debugging, which can be written to a file.
    InternalError T.Text T.Text ErrorClass

instance Show CompilerError where
  show (ExternalError s) = pretty s
  show (InternalError s _ _) = T.unpack s

-- | Raise an 'ExternalError' based on a prettyprinting result.
externalError :: MonadError CompilerError m => Doc -> m a
externalError = throwError . ExternalError

-- | Raise an 'ExternalError' based on a string.
externalErrorS :: MonadError CompilerError m => String -> m a
externalErrorS = externalError . text

-- | Raise an v'InternalError' based on a prettyprinting result.
internalErrorS :: MonadError CompilerError m => String -> Doc -> m a
internalErrorS s d =
  throwError $ InternalError (T.pack s) (prettyText d) CompilerBug

-- | An error that is not the users fault, but a bug (or limitation)
-- in the compiler.  Compiler passes should only ever report this
-- error - any problems after the type checker are *our* fault, not
-- the users.  These are generally thrown as IO exceptions, and caught
-- at the top level.
data InternalError = Error ErrorClass T.Text
  deriving (Show)

instance Exception InternalError

-- | Throw an t'InternalError' that is a 'CompilerBug'.
compilerBug :: T.Text -> a
compilerBug = throw . Error CompilerBug

-- | Throw an t'InternalError' that is a 'CompilerLimitation'.
compilerLimitation :: T.Text -> a
compilerLimitation = throw . Error CompilerLimitation

-- | Like 'compilerBug', but with a 'String'.
compilerBugS :: String -> a
compilerBugS = compilerBug . T.pack

-- | Like 'compilerLimitation', but with a 'String'.
compilerLimitationS :: String -> a
compilerLimitationS = compilerLimitation . T.pack
