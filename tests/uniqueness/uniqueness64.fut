-- Piping into a function with a unique return type yields a fresh value, just
-- as applying it directly does.  This requires the type checker and the
-- defunctionaliser to agree; see Note [Parametric results] in
-- Language.Futhark.TypeChecker.Consumption.
-- ==
-- input { [1,2,3] }
-- output { [3,2,1] [3,2,1] [3,2,1] }

def viapipe (xs: []i32) : *[]i32 = xs |> copy

def viabackpipe (xs: []i32) : *[]i32 = copy <| xs

entry main (xs: []i32) : ([]i32, []i32, []i32) =
  (reverse (viapipe xs), reverse (viabackpipe xs), reverse (copy xs))
