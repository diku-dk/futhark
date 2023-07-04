-- Issue #1975
-- ==
-- error: aliased to some other component

def main n : (*[]i64, *[]i64) =
  let (foo,bar) =
    loop _ = (iota 10,iota 10) for i < n do
    let arr = iota 10
    in (arr,arr)
  in (foo,bar)
