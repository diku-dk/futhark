let render (color_fun : i32 -> i32) (h : i32) (w: i32) : []i32 =
  tabulate h (\i -> color_fun i)

let get [n] (arr: [n][n]i32) (i : i32) : i32 =
  arr[i,i]

let main [n] mode (arr: [n][n]i32) =
  if mode then [] else
  render (get arr) n n
