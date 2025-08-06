-- ==
-- input { [102u8, 111u8, 111u8, 32u8, 98u8, 97u8, 114u8, 32u8, 98u8, 97u8, 122u8] 1 }
-- output { [98u8, 97u8, 114u8] }

def segmented_scan 't [n] (g: t -> t -> t) (ne: t) (flags: [n]bool) (vals: [n]t) : [n]t =
  let pairs =
    scan (\(v1, f1) (v2, f2) ->
            let f = f1 || f2
            let v = if f2 then v2 else g v1 v2
            in (v, f))
         (ne, false)
         (zip vals flags)
  let (res, _) = unzip pairs
  in res

def is_space (x: u8) = x == ' '
def isnt_space x = !(is_space x)

def (&&&) f g = \x -> (f x, g x)

module tokens
  : {
      type token [w]
      val tokens [n] : [n]u8 -> ?[k][w].(token [w] -> ?[m].[m]u8, [k](token [w]))
    } = {
  type token [w] = ([w](), i64, i64)

  def tokens [n] (s: [n]u8) =
    let rep = replicate 0 ()
    in ( \(_, i, k) -> #[unsafe] s[i:i + k]
       , segmented_scan (+) 0 (map is_space s) (map (isnt_space >-> i64.bool) s)
         |> (id &&& rotate 1)
         |> uncurry zip
         |> zip (indices s)
         |> filter (\(_, (x, y)) -> x > y)
         |> map (\(i, (x, _)) -> (rep, i - x + 1, x))
       )
}

def main xs i =
  let (f, ts) = tokens.tokens xs
  in f ts[i]
