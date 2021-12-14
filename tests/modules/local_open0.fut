-- Does local open work at all?
-- ==
-- input { 1 } output { 6 }

module m = {
def x = 2
def y = 3
}

def main(x: i32) = x + m.(x + y)
