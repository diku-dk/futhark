-- http://rosettacode.org/wiki/Averages/Root_mean_square
--
-- input { [1.0,2.0,3.0,1.0] }
-- output { 1.936f64 }

import "futlib/numeric"

fun main(as: [n]f64): f64 =
  F64.sqrt ((reduce (+) 0.0 (map (**2.0) as)) / f64(n))
