-- Lifting a simple function
-- ==
-- entry: main
-- input  { [0i64, 1i64, 2i64, 3i64, 4i64, 5i64] }
-- output { [1i64, 2i64, 3i64, 4i64, 5i64, 6i64] }
-- input  { empty([0]i64) }
-- output { empty([0]i64) }


#[noinline]
def bar (x : i64) = x + 1

#[noinline]
def foo (x : i64) = bar x

def main (xs : []i64) = map foo xs
