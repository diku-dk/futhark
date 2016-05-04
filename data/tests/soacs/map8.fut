-- Test whether the internaliser can properly handle very complex map
-- return types.
-- ==

fun int main([(int,int)] a) =
  let b = map(fn ([(int,int)],[(int,int)]) (int x, int y) =>
                (zip(iota(x),iota(y)),zip(iota(x),iota(y))),
              a) in
  0
