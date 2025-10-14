type t = #foo | #bar

def main (x: i32) =
  match #foo : t
  case #foo ->
    let xs = filter (> x) [1, 2, 3]
    in length xs
  case #bar ->
    0
