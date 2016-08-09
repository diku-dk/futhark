-- Does simple tuple projection work?
--
-- ==
-- compiled input { 1i32 2i8 3i16 }
-- output { 2i8 3i16 1i32 }

fun (i8,i16,i32) main((i32,i8,i16) x) =
  (x.1, x.2, x.0)
