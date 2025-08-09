-- ==
-- error: Consuming.*"empty"

module Test = {
  def empty = []
}

def consume (arr: *[]i64) = arr

entry main (_: bool) = consume Test.empty
