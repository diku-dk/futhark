-- Sumtype consumption.
-- ==
-- error: "v".*consumed

type^ sum = #foo (*[]i32) | #bar (*[]i32)

def main (v: *sum) =
  let x =
    match v
    case #foo v -> v with [0] = 0
    case #bar v -> v
  in v
