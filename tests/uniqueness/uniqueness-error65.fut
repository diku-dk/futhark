-- From #2067.
-- ==
-- error: "xs".*not consumable

def main (xs: *[]i32) =
  loop xs : []i32 for i < 10 do
    xs with [i] = i+1
