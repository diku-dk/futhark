{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Futhark.Pass.ExplicitAllocations.Seq
       ( explicitAllocations
       , simplifiable
       )
where

import Futhark.Pass
import Futhark.Representation.SeqMem
import Futhark.Representation.Seq
import Futhark.Pass.ExplicitAllocations

explicitAllocations :: Pass Seq SeqMem
explicitAllocations = explicitAllocationsGeneric undefined defaultExpHints
