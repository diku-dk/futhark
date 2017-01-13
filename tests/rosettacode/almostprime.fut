-- http://rosettacode.org/wiki/Almost_prime
--
-- ==
-- input { 2 }
-- output { [[2i32, 3i32, 5i32, 7i32, 11i32, 13i32, 17i32, 19i32, 23i32, 29i32],
--           [4i32, 6i32, 9i32, 10i32, 14i32, 15i32, 21i32, 22i32, 25i32, 26i32]] }


fun kprime(n: int, k: int): bool =
  let (p,f) = (2, 0)
  loop ((n, p, f)) = while f < k && p*p <= n do
    loop ((n,f)) = while 0 == n % p do
      (n/p, f+1)
    in (n, p+1, f)
  in f + (if n > 1 then 1 else 0) == k

fun main(m: int): [][]int =
  map (\k: [10]int ->
         let ps = replicate 10 0
         loop ((i,c,ps) = (2,0,ps)) = while c < 10 do
           if kprime(i,k) then
             unsafe let ps[c] = i
                    in (i+1, c+1, ps)
           else (i+1, c, ps)
         in ps)
  (map (1+) (iota m))
