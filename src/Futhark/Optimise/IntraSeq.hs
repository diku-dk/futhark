module Futhark.Optimise.IntraSeq
    (intraSeq)
where

import Futhark.Pass
import Futhark.IR.GPU

intraSeq :: Pass GPU GPU
intraSeq =
    Pass
        {
            passName = "Det bedste pass",
            passDescription = "Læs navnet",
            passFunction = pure
        }