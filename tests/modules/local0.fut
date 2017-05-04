-- Test that something defined with local is not accessible outside the module.
-- ==
-- input {} output { 1 }

let x = 1

open { local let x = 2 }

let main() = x
