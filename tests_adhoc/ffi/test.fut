type r = {
    x: i32,
    y: i32
}
type s = #a i32 | #b i32 | #c i32

entry p1: i32 = 1i32
entry p2: i32 = 2i32
entry p3: i32 = 3i32

entry r1: r = {x = 1i32, y = 2i32}
entry r2: r = {x = 3i32, y = 4i32}
entry r3: r = {x = 5i32, y = 6i32}

entry s1: s = #a 2i32
entry s2: s = #b 4i32
entry s3: s = #c 6i32

entry pa1: [3]i32 = [p1, p2, p3]
entry pa2: [2][3]i32 = [[p1, p2, p3], [p3, p2, p1]]

entry ra1: [3]r = [r1, r2, r3]
entry ra2: [2][3]r = [[r1, r2, r3], [r3, r2, r1]]

entry sa1: [3]s = [s1, s2, s3]
entry sa2: [2][3]s = [[s1, s2, s3], [s3, s2, s1]]

entry pf (x: i32): i32 = x ** 2
entry rf (x: r): r = { x = x.x ** 2, y = x.y + 2 }
entry sf (x: s): s =
  match x
    case #a v -> #c (v + 1)
    case #b v -> #b (v + 2)
    case #c v -> #a (v + 3)

entry pa1f (x: []i32): []i32 = map (**2) x
entry pa2f (x: [][]i32): [][]i32 = let v = map2 (**) x[0,:] x[1,:] in [v,v]
