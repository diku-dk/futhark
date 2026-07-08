#!/usr/bin/env runhaskell

import Distribution.Simple

main :: IO ()
main = defaultMainWithHooks myHooks
  where
    myHooks = simpleUserHooks
