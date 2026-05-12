-- Only one of the accesses should be unsafe.
--
-- ==
-- structure { Assert 1 }

def main (a: []i32, i: i32, j: i32) : (i32, i32) =
  (#[unsafe] a[i], a[j])
