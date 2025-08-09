type sumType = #some ([0]i32) | #none

entry main =
  (\(x: sumType) ->
     match x
     case (#some y) -> id y
     case _ -> []) (#none : sumType)
