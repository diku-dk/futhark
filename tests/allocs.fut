-- Nasty program that tries to leak memory.  If we can run this
-- without leaking, then we're doing well.
-- ==
-- input { [0, 1000, 42, 1001, 50000] }
-- output { 1300103225i32 }

let main [n] (a: [n]i32): i32 =
  let b = loop b = iota(10) for i < n do
    (let m = a[i]
     in if m < length b
        then b
        else map (\(j: i32): i32  ->
                   j + unsafe b[j % length b]) (
                 iota(m)))
  in reduce (+) 0 b
