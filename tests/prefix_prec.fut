-- Test that prefix operators have the right precedence.
--
-- ==
-- input {1 2} output { 1 }

let main x y = -x%y : i32
