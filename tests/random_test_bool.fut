-- Can we generate random boolean data?  (Bools have strange
-- representations for some backends.)
-- ==
-- random input { [100]bool } auto output

let main (bs: []bool) = reduce (&&) true bs
