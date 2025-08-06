-- ==
-- input {
--   10 21
-- }
-- output {
--   31
-- }

module IntLib = {
  def plus (a: i32, b: i32) : i32 = a + b
  def numberFour () : i32 = 4
}

def localplus (a: i32, b: i32) : i32 = IntLib.plus (a, b)

def main (a: i32) (b: i32) : i32 =
  localplus (a, b)
