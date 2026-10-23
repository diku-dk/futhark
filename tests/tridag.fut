--------------------------------------------
--subroutine tridag(a,b,c,d,nn)
--
--    dimension a(nn),b(nn),c(nn),d(nn)
--
--    if(nn .eq. 1) then
--        d(1)=d(1)/b(1)
--        return
--    end if
--
--    do 10 k = 2,nn
--        xm = a(k)/b(k-1)
--        b(k) = b(k) - xm*c(k-1)
--        d(k) = d(k) - xm*d(k-1)
--10  continue
--
--    d(nn) = d(nn)/b(nn)
--    k = nn
--    do 20 i = 2,nn
--        k = nn + 1 - i
--        d(k) = (d(k) - c(k)*d(k+1))/b(k)
--20  continue
--    return
--end
--------------------------------------------/
-- ==
-- input {
-- }
-- output {
--   [1.000000f32, 0.335000f32, -13.003881f32, 4.696531f32, 2.284400f32, -1.201104f32, 23.773315f32,
--    6.997077f32, 5.064199f32, 3.832115f32]
--   [-3.039058f32, 6.578116f32, -1.103211f32, -6.002021f32, 8.084014f32, -3.267883f32, -0.332640f32,
--    2.726331f32, -1.618253f32, 1.472878f32]
-- }

def tridag [nn]
           ( b: *[]f32
           , d: *[nn]f32
           , a: []f32
           , c: []f32
           ) : ([]f32, []f32) =
  if (nn == 1)
  then --then ( b, map(\f32 (f32 x, f32 y) -> x / y, d, b) )
       (b, [d[0] / b[0]])
  else let (b, d) =
         loop ((b, d)) for i < (nn - 1) do
           let xm = a[i + 1] / b[i]
           let b[i + 1] = b[i + 1] - xm * c[i]
           let d[i + 1] = d[i + 1] - xm * d[i]
           in (b, d)
       let d[nn - 1] = d[nn - 1] / b[nn - 1]
       let d =
         loop (d) for i < (nn - 1) do
           let k = nn - 2 - i
           let d[k] = (d[k] - c[k] * d[k + 1]) / b[k]
           in d
       in (b, d)

def main : ([]f32, []f32) =
  let nn = reduce (+) 0 ([1, 2, 3, 4])
  let a = replicate nn 3.33
  let b = map (\x -> f32.i64 (x) + 1.0) (iota (nn))
  let c = map (\x -> 1.11 * f32.i64 (x) + 0.5) (iota (nn))
  let d = map (\x -> 1.01 * f32.i64 (x) + 0.25) (iota (nn))
  in tridag (b, d, a, c)
