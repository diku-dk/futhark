type~ sumT = #foo []i64 | #bar i64

def thing xs : sumT = #foo (filter (> 0) xs)

def main (xs: []i64) : []i64 =
  match thing xs
  case #foo xs' -> xs ++ xs'
  case #bar i -> xs ++ [i]
