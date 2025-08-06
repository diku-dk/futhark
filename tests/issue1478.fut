-- ==
-- error: Entry points

module mmain = {
  entry f x = x + 1
}

entry f x = mmain.f x
