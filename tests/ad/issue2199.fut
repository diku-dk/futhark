-- ==
-- entry: test_primal test_rev
-- input { [1.0,2.0] [3.0,4.0] [5.0, 6.0] }
-- output { 3.0 7.0 11.0 }

def op (x0, y0, z0) (x1, y1, z1) : (f64, f64, f64) = (x0 + x1, y0 + y1, z0 + z1)
def ne = (0f64, 0f64, 0f64)

def primal xs = reduce_comm op ne xs

entry test_primal as bs cs = primal (zip3 as bs cs)

entry test_rev as bs cs =
  (vjp2 (\(as, bs, cs) -> test_primal as bs cs) (as, bs, cs) (1, 1, 1)).0
