-- We cannot have a parameter with an array of functions.
-- ==
-- error: Cannot .* array with elements of lifted type .* -> .*

def f (arr: [](i32 -> i32)) = arr
