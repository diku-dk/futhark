-- Indexing through a record field in a single update path.
-- ==
-- input { 0i32 }
-- output { [1i32, 0i32, 3i32] }

entry main (v: i32) : [3]i32 =
  let xss: *[]{a: []i32} = [{a = [1i32, 2i32, 3i32]}, {a = [4i32, 5i32, 6i32]}]
  in (xss with [0].a[1] = v)[0].a
