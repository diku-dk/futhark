-- ==
-- error: Consuming.*"empty"

module Test = {
    let empty = []
}

let consume (arr: *[]i64) = arr

entry main (_: bool) = consume Test.empty
