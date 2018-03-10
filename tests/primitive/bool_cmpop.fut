-- Test comparison of boolean values.
--
-- ==
-- input { false false } output { false false true  true  true  }
-- input { false true  } output { true  true  false true  true  }
-- input { true  false } output { false false false false false }
-- input { false false } output { false false true  true  true  }

let main(x: bool, y: bool) =
  (x < y, y > x, x == y, x <= y, y >= x)
