-- ==
-- input {
--   10 21
-- }
-- output {
--   6
-- }

type t = i32

module NumLib = {
  def plus (a: t, b: t) : t = a + b

  module BestNumbers = {
    def four () : t = 4
    def seven () : t = 42
    def six () : t = 41
  }
}

def localplus (a: i32, b: i32) : i32 = NumLib.plus (a, b)

def main (a: i32) (b: i32) : i32 =
  localplus (NumLib.BestNumbers.four (), 2)
