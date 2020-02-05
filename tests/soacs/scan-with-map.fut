-- This one is tricky to get to run without too many memory copies.
-- When it was added, in-place-lowering couldn't get it to work right.
--
-- Now it also functions as a test for whether scans with higher-order
-- operators work.  Note that it is possible the scan is interchanged
-- with the inner map during fusion or kernel extraction.
--
-- ==
-- tags { no_python }
-- compiled input { [1,2,3] 100001 } output { 366240i32 }

let main [n] (a: [n]i32) (m: i32): i32 =
  let contribs = replicate m a
  let res = scan (map2 (+)) a contribs
  in reduce (^) 0 (flatten res)
