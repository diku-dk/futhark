-- Basic tests, but then again, this is a basic library.
--
-- OpenCL execution is disabled.  Not because this program can't run
-- in OpenCL (it's all scalar code), but because it takes forever due
-- to the need to start up the OpenCL runtime for each of the (many)
-- test cases.
--
-- ==
-- tags { no_opencl }

module date = import "futlib/date"

-- ==
-- entry: date_triples_inv
-- input { 1 2 3 } output { 1 2 3 }

entry date_triples_inv (x:i32) (y:i32) (z:i32) =
  date.triple_of_date (date.date_of_triple (x,y,z))

-- ==
-- entry: check_date
-- input { 1 2 3 } output { true }
-- input { 1996 2 29 } output { true }
-- input { 1997 2 29 } output { false }
-- input { 2000 2 29 } output { true }
-- input { 1900 2 29 } output { false }
-- input { 2400 2 29 } output { true }
-- input { 1600 2 29 } output { true }
-- input { 2017 1 31 } output { true }
-- input { 2017 1 0 } output { false }

entry check_date (x:i32) (y:i32) (z:i32) =
  date.check_date (x,y,z)

-- ==
-- entry: add_days
-- input { 1 2 3 1 } output { 1 2 4 }
-- input { 1996 2 28 1 } output { 1996 2 29 }
-- input { 1996 2 28 2 } output { 1996 3 1 }
-- input { 1996 12 31 1 } output { 1997 1 1 }
-- input { 1997 1 1 -1 } output { 1996 12 31 }
-- input { 1997 1 1 -1 } output { 1996 12 31 }

entry add_days (x:i32) (y:i32) (z:i32) (d:i32) =
  date.triple_of_date (date.add_days (date.date_of_triple (x,y,z)) d)

-- ==
-- entry: sub_days
-- input { 1 2 3 1 } output { 1 2 2 }
-- input { 1996 3 1 1 } output { 1996 2 29 }
-- input { 1996 12 31 -1 } output { 1997 1 1 }
-- input { 1997 1 1 1 } output { 1996 12 31 }
-- input { 1997 1 1 1 } output { 1996 12 31 }

entry sub_days (x:i32) (y:i32) (z:i32) (d:i32) =
  date.triple_of_date (date.sub_days (date.date_of_triple (x,y,z)) d)

-- ==
-- entry: diff_dates
-- input { 1 2 3  1 2 4 } output { 0.002740 }
-- input { 1 2 3  2 2 3 } output { 1.0 }

entry diff_dates (x1:i32) (y1:i32) (z1:i32) (x2:i32) (y2:i32) (z2:i32) =
  let t1 = date.date_of_triple (x1,y1,z1)
  let t2 = date.date_of_triple (x2,y2,z2)
  in date.diff_dates t1 t2
