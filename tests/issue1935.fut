def get_name (i: i64) =
  match i
  case 0 -> "some name"
  case _ -> ""

entry main =
  loop i = 0
  while length (get_name i) != 0 do
    i + 1
