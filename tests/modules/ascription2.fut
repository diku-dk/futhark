-- Test ascriptions of signatures with type abbreviations referencing
-- abstract types.
-- ==
-- input {} output { 3 }

module type T1 = { type t type s = t val a : s }
module X : T1 = { type t = i32 type s = i32 let a : s = 3 }    -- ok
fun main () : i32 = 3
