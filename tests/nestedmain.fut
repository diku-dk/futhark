-- ==
-- input { true } output { false }

module m = {
    let main (x: i32) = x + 2
}

def main b : bool = !b
