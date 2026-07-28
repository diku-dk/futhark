module Language.Futhark.Interpreter.FFI.AtomicList
  ( AtomicList,
    new,
    prepend,
    flush,
  )
where

import Data.IORef (IORef, atomicModifyIORef, newIORef)

newtype AtomicList v = AtomicList (IORef [v])

new :: IO (AtomicList v)
new = AtomicList <$> newIORef []

prepend :: v -> AtomicList v -> IO ()
prepend v (AtomicList r) = atomicModifyIORef r $ (,()) . (v :)

flush :: AtomicList v -> IO [v]
flush (AtomicList r) = atomicModifyIORef r ([],)
