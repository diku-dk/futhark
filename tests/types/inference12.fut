-- Type inference based on SOAC usage.
-- ==
-- input { [true, false] } output { false }

def main xs = reduce (&&) true xs
