-- This program broke the simplifier at one point.

def main (x: i32, y: i32, a: []i32, b: []i32) : []i32 =
  let c =
    map (\(av: i32) : (i32, i32) ->
           let v = x + y
           in (v, 2 * av))
        a
  in map (\(x, y) -> x + y) c
