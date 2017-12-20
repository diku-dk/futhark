{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Definition of a polymorphic (generic) pass that can work with programs of any
-- lore.
module Futhark.Pass
       ( PassM
       , runPassM
       , liftEither
       , liftEitherM
       , Pass (..)
       , passLongOption
       , simplePass
       ) where

import Control.Monad.Writer.Strict
import Control.Monad.Except
import Control.Monad.State
import Data.Char
import qualified Data.Text as T

import Futhark.Representation.AST
import Futhark.Util.Log
import Futhark.MonadFreshNames

-- | The monad in which passes execute.
newtype PassM a = PassM (ExceptT T.Text (WriterT Log (State VNameSource)) a)
              deriving (Functor, Applicative, Monad,
                        MonadError T.Text)

instance MonadLogger PassM where
  addLog = PassM . tell

instance MonadFreshNames PassM where
  putNameSource = PassM . put
  getNameSource = PassM get

-- | Execute a 'PassM' action, yielding logging information and either
-- an error text or a result.
runPassM :: MonadFreshNames m =>
            PassM a -> m (Either T.Text a, Log)
runPassM (PassM m) = modifyNameSource $ \src ->
  runState (runWriterT $ runExceptT m) src

-- | Turn an 'Either' computation into a 'PassM'.  If the 'Either' is
-- 'Left', the result is an exception.
liftEither :: Show err => Either err a -> PassM a
liftEither (Left e)  = throwError $ T.pack $ show e
liftEither (Right v) = return v

-- | Turn an 'Either' monadic computation into a 'PassM'.  If the 'Either' is
-- 'Left', the result is an exception.
liftEitherM :: Show err => PassM (Either err a) -> PassM a
liftEitherM m = liftEither =<< m

-- | A compiler pass transforming a 'Prog' of a given lore to a 'Prog'
-- of another lore.
data Pass fromlore tolore =
  Pass { passName :: String
         -- ^ Name of the pass.  Keep this short and simple.  It will
         -- be used to automatically generate a command-line option
         -- name via 'passLongOption'.
       , passDescription :: String
         -- ^ A slightly longer description, which will show up in the
         -- command-line help text.
       , passFunction :: Prog fromlore -> PassM (Prog tolore)
       }

-- | Take the name of the pass, turn spaces into dashes, and make all
-- characters lowercase.
passLongOption :: Pass fromlore tolore -> String
passLongOption = map (spaceToDash . toLower) . passName
  where spaceToDash ' ' = '-'
        spaceToDash c   = c

-- | Turn a simple function on programs into a pass that cannot fail
-- and produces no log output.
simplePass :: String -> String
           -> (Prog fromlore -> State VNameSource (Prog tolore))
           -> Pass fromlore tolore
simplePass name desc f = Pass { passName = name
                              , passDescription = desc
                              , passFunction = modifyNameSource . runState . f
                              }
