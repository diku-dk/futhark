-- A test specification is untrusted code, so it must not be able to write
-- files.
-- ==
-- script input { let _ = io.savevalue "evil.bin" [1i32] in [1i32, 2, 3] }
-- output { 6i32 }

def main (xs: []i32) = i32.sum xs
