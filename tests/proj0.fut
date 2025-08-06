-- Does simple tuple projection work?
--
-- ==
-- compiled input { 1i32 2i8 3i16 }
-- output { 2i8 3i16 1i32 }

def main (x0: i32) (x1: i8) (x2: i16) : (i8, i16, i32) =
  let x = (x0, x1, x2)
  in (x.1, x.2, x.0)
