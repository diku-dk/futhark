-- Test simple indexing of an array.
-- ==
-- tags { firsttag secondtag }
-- input { [4,3,2,1] 1i64 }
-- output { 3 }
-- input { [4,3,2,1] 5i64 }
-- error: Error*

def main (a: []i32) (i: i64) : i32 =
  a[i]
