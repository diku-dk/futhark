module L0C.EnablingOpts.ScalExp
  ( RelOp0(..)
  , ScalExp(..)
  )
where

import L0C.InternalRep

-----------------------------------------------------------------
-- BINARY OPERATORS for Numbers                                --
-- Note that MOD, BAND, XOR, BOR, SHIFTR, SHIFTL not supported --
--   `a SHIFTL/SHIFTR p' can be translated if desired as as    --
--   `a * 2^p' or `a / 2^p                                     --
-----------------------------------------------------------------

-- | Relational operators.
data RelOp0 = LTH0
            | LEQ0
         -- | EQL0
             deriving (Eq, Ord, Enum, Bounded, Show)

-- | Representation of a scalar expression, which is:
--
--    (i) an algebraic expression, e.g., min(a+b, a*b),
--
--   (ii) a relational expression: a+b < 5,
--
--  (iii) a logical expression: e1 and (not (a+b>5)
data ScalExp= Val     BasicValue
            | Id      Ident
            | SNeg    ScalExp
            | SNot    ScalExp
            | SPlus   ScalExp ScalExp
            | SMinus  ScalExp ScalExp
            | STimes  ScalExp ScalExp
            | SPow    ScalExp ScalExp
            | SDivide ScalExp ScalExp
            | MaxMin  Bool   [ScalExp]
            | RelExp  RelOp0  ScalExp
            | SLogAnd ScalExp ScalExp
            | SLogOr  ScalExp ScalExp
              deriving (Eq, Ord, Show)
