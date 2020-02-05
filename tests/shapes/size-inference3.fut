-- We cannot constrain the inferred size of an array parameter to a
-- size that will not be visible in the function signature.
-- ==
-- error:

let main xs = zip xs (iota xs[0])
