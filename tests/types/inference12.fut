-- Type inference based on SOAC usage.
-- ==
-- input { [true, false] } output { false }

let main xs = reduce (&&) true xs
