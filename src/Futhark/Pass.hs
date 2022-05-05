{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Strict #-}

-- | Definition of a polymorphic (generic) pass that can work with
-- programs of any rep.
module Futhark.Pass
  ( PassM,
    runPassM,
    liftEither,
    liftEitherM,
    Pass (..),
    passLongOption,
    parPass,
    intraproceduralTransformation,
    intraproceduralTransformationWithConsts,
  )
where

import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Parallel.Strategies
import Data.Char
import Data.Either
import Futhark.Error
import Futhark.IR
import Futhark.MonadFreshNames
import Futhark.Util.Log
import Prelude hiding (log)

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
runPassM ::
  MonadFreshNames m =>
  PassM a ->
  m (a, Log)
runPassM (PassM m) = modifyNameSource $ runState (runWriterT m)

-- | Turn an 'Either' computation into a 'PassM'.  If the 'Either' is
-- 'Left', the result is a 'CompilerBug'.
liftEither :: Show err => Either err a -> PassM a
liftEither (Left e) = compilerBugS $ show e
liftEither (Right v) = pure v

-- | Turn an 'Either' monadic computation into a 'PassM'.  If the 'Either' is
-- 'Left', the result is an exception.
liftEitherM :: Show err => PassM (Either err a) -> PassM a
liftEitherM m = liftEither =<< m

-- | A compiler pass transforming a 'Prog' of a given rep to a 'Prog'
-- of another rep.
data Pass fromrep torep = Pass
  { -- | Name of the pass.  Keep this short and simple.  It will
    -- be used to automatically generate a command-line option
    -- name via 'passLongOption'.
    passName :: String,
    -- | A slightly longer description, which will show up in the
    -- command-line help text.
    passDescription :: String,
    passFunction :: Prog fromrep -> PassM (Prog torep)
  }

-- | Take the name of the pass, turn spaces into dashes, and make all
-- characters lowercase.
passLongOption :: Pass fromrep torep -> String
passLongOption = map (spaceToDash . toLower) . passName
  where
    spaceToDash ' ' = '-'
    spaceToDash c = c

-- | Apply a 'PassM' operation in parallel to multiple elements,
-- joining together the name sources and logs, and propagating any
-- error properly.
parPass :: (a -> PassM b) -> [a] -> PassM [b]
parPass f as = do
  (x, log) <- modifyNameSource $ \src ->
    let (bs, logs, srcs) = unzip3 $ parMap rpar (f' src) as
     in ((bs, mconcat logs), mconcat srcs)

  addLog log
  pure x
  where
    f' src a =
      let ((x', log), src') = runState (runPassM (f a)) src
       in (x', log, src')

-- | Apply some operation to the top-level constants. Then applies an
-- operation to all the function definitions, which are also given the
-- transformed constants so they can be brought into scope.
-- The function definition transformations are run in parallel (with
-- 'parPass'), since they cannot affect each other.
intraproceduralTransformationWithConsts ::
  (Stms fromrep -> PassM (Stms torep)) ->
  (Stms torep -> FunDef fromrep -> PassM (FunDef torep)) ->
  Prog fromrep ->
  PassM (Prog torep)
intraproceduralTransformationWithConsts ct ft (Prog consts funs) = do
  consts' <- ct consts
  funs' <- parPass (ft consts') funs
  pure $ Prog consts' funs'

-- | Like 'intraproceduralTransformationWithConsts', but do not change
-- the top-level constants, and simply pass along their 'Scope'.
intraproceduralTransformation ::
  (Scope rep -> Stms rep -> PassM (Stms rep)) ->
  Prog rep ->
  PassM (Prog rep)
intraproceduralTransformation f =
  intraproceduralTransformationWithConsts (f mempty) f'
  where
    f' consts fd = do
      stms <-
        f
          (scopeOf consts <> scopeOfFParams (funDefParams fd))
          (bodyStms $ funDefBody fd)
      pure fd {funDefBody = (funDefBody fd) {bodyStms = stms}}
