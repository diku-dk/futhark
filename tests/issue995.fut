def render (color_fun: i64 -> i32) (h: i64) (w: i64) : []i32 =
  tabulate h (\i -> color_fun i)

def get [n] (arr: [n][n]i32) (i: i64) : i32 =
  arr[i, i]

def main [n] mode (arr: [n][n]i32) =
  if mode
  then []
  else render (get arr) n n
