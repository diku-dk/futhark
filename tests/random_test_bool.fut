-- Can we read random boolean data?  (Bools have strange
-- representations for some backends.)
-- ==
-- random input { [100]bool } auto output

def main (bs: []bool) = reduce (&&) true bs
