-- | A single data type, exported here so the compiler can include it
-- without depending on the generated parser source.  This makes it easier to load the bulk of Futhark in @ghci@.
module Language.Futhark.Parser.RealConfiguration
       (RealConfiguration (..))
       where

-- | Whether the type 'real' should be interpreted as 'float32' or 'float64'.
data RealConfiguration = RealAsFloat32 | RealAsFloat64
