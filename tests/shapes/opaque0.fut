-- ==
-- error: do not match

module num
  : {
      type t [n]
      val mk : (x: i64) -> t [x]
      val un [n] : t [n] -> i64
      val comb [n] : t [n] -> t [n] -> i64
    } = {
  type t [n] = [n]()
  def mk x = replicate x ()
  def un x = length x
  def comb x y = length (zip x y)
}

def f x =
  let y = x + 1
  in num.mk y

def main a b =
  num.comb (f a) (f b)
