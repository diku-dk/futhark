-- Derived from #1535.
-- ==
-- error: aliased to "x"

type sumType = #some ([0]i32) | #none

entry main =
  (\(x: sumType) : *[]i32 ->
     match x
     case (#some y) -> id y
     case _ -> []) (#none : sumType)
