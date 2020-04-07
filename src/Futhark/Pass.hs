{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Definition of a polymorphic (generic) pass that can work with programs of any
-- lore.
module Futhark.Pass
       ( PassM
       , runPassM
       , liftEither
       , liftEitherM
       , Pass (..)
       , passLongOption
       , intraproceduralTransformation
       ) where

import Control.Monad.Writer.Strict
import Control.Monad.State.Strict
import Control.Parallel.Strategies
import Data.Char
import Data.Either

import Prelude hiding (log)

import Futhark.Error
import Futhark.Representation.AST
import Futhark.Util.Log
import Futhark.MonadFreshNames

-- | The monad in which passes execute.
newtype PassM a = PassM (WriterT Log (State VNameSource) a)
              deriving (Functor, Applicative, Monad)

instance MonadLogger PassM where
  addLog = PassM . tell

instance MonadFreshNames PassM where
  putNameSource = PassM . put
  getNameSource = PassM get

-- | Execute a 'PassM' action, yielding logging information and either
-- an error text or a result.
runPassM :: MonadFreshNames m =>
            PassM a -> m (a, Log)
runPassM (PassM m) = modifyNameSource $ runState (runWriterT m)

-- | Turn an 'Either' computation into a 'PassM'.  If the 'Either' is
-- 'Left', the result is a 'CompilerBug'.
liftEither :: Show err => Either err a -> PassM a
liftEither (Left e)  = compilerBugS $ show e
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

intraproceduralTransformation :: (FunDef fromlore -> PassM (FunDef tolore))
                              -> Prog fromlore -> PassM (Prog tolore)
intraproceduralTransformation ft prog = do
  (x, log) <- modifyNameSource $ \src ->
    let (funs, logs, srcs) = unzip3 $ parMap rpar (onFunction src) (progFuns prog)
    in ((Prog funs, mconcat logs), mconcat srcs)

  addLog log
  return x

  where onFunction src f =
          let ((x, log), src') = runState (runPassM (ft f)) src
          in (x, log, src')
