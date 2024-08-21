module Futhark.Analysis.Proofs.Util
where

import Data.Set qualified as S

class Ord a => FreeIn a where
  freeIn :: a -> S.Set VName

class Renameable u where
  -- Rename bound variables in u.
  rename :: u -> u -- Implement subC(id,id,u) from Sieg and Kaufmann.
