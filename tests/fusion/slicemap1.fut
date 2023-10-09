-- ==
-- structure { Screma 1 }

def main (xs: []i32) =
  map (+3) ((map (+2) xs)[::2])
