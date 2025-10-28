-- Another complicated case.
-- ==
-- input { 1i64 2i64 }
-- output { [[true, true, true], [true, true, true], [true, true, true]] }

def plus a b : i64 = a + b

def plus_comm [a] [b] 't : [plus a b][plus b a]bool = tabulate_2d (plus a b) (plus b a) (\_ _ -> true)

def main a b = copy plus_comm : [plus a b][plus b a]bool
