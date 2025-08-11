-- ==
-- error: Loop body does not have expected type.*"n" and "3" do not match

def main n : [n]bool =
  loop (arr: [n]bool) = replicate n true
  for i < 10 do
    replicate 3 true
