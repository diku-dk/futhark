-- One branch returns a value whose components coincide; the other consumes
-- what they coincide in.  The join consumes both, so one component of the
-- result can still be consumed.
-- ==
-- input { true [1,2,3] [4,5,6] [7,8,9] } output { [0,2,3] }
-- input { false [1,2,3] [4,5,6] [7,8,9] } output { [0,5,6] }

def main (c: bool) (u: *[]i32) (a: *[]i32) (b: *[]i32) =
  let p = (u, u)
  let (r0, _) = if c then p else (let _ = p.0 with [0] = 5 in (a, b))
  in r0 with [0] = 0
