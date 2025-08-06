-- ==
-- input {
-- }
-- output {
--   [[2,4,5],[1,5,3],[3,7,1]]
-- }

def min (a: i32) (b: i32) : i32 = if (a < b) then a else b
def plus1 (a: []i32, b: []i32) : []i32 = [1]

module M0 = {
  def min1 [n] (a: [n]i32, b: [n]i32) = map2 min a b
  def redmin1 (a: []i32) : i32 = reduce min 1200 a
  def redmin2 [n] (a: [n][]i32) : [n]i32 = map redmin1 a

  module M1 = {
    def plus1 [n] (a: [n]i32, b: [n]i32) : [n]i32 = map2 (+) a b
    def plus2 [n] [k] (a: [n][k]i32, b: [n][k]i32) : [n][k]i32 = map plus1 (zip a b)
  }

  def replin [k] (len: i64) (a: [k]i32) : [len][k]i32 = replicate len a
}

def floydSbsFun (n: i64) (d: [n][n]i32) : [][]i32 =
  let d3 = replicate n (transpose d)
  let d2 = map (M0.replin n) d
  let abr = map M0.M1.plus2 (zip d3 d2)
  let partial = map M0.redmin2 abr
  in map M0.min1 (zip partial d)

def main : [][]i32 =
  let arr = [[2, 4, 5], [1, 1000, 3], [3, 7, 1]]
  in floydSbsFun 3 arr
