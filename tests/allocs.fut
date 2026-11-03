-- Nasty program that tries to leak memory.  If we can run this
-- without leaking, then we're doing well.
-- ==
-- compiled input { [0, 1000, 42, 1001, 50000] }
-- output { 1300103225i64 }

def main [n] (a: [n]i32) : i64 =
  let b =
    loop b = iota (10)
    for i < n do
      (let m = i64.i32 a[i]
       in if m < length b
          then b
          else map (\j ->
                      j + b[j % length b])
                   (iota (m)))
  in reduce (+) 0 b
