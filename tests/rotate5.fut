-- Does rotate work even if we do something with the array afterwards?
-- This is particularly a test of how this is simplified.
--
-- ==
-- input { 8i64 }
-- output { [8i64, 1i64, 2i64, 3i64, 4i64, 5i64, 6i64, 7i64] }


def main(i: i64): []i64 =
  let a = iota(i)
  in map (1+) (rotate (-1) a)
