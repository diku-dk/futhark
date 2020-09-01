{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Futhark.Pass.ExplicitAllocations.Seq
  ( explicitAllocations,
    simplifiable,
  )
where

import Futhark.IR.Seq
import Futhark.IR.SeqMem
import Futhark.Pass
import Futhark.Pass.ExplicitAllocations

explicitAllocations :: Pass Seq SeqMem
explicitAllocations = explicitAllocationsGeneric (pure . Inner) defaultExpHints
