-- Test program for issue #194.
--
-- Revealed a bug in in-place-lowering.
-- ==
-- tags { no_opencl }

fun [[int, numBins2]] main([f32, numD] points,
                           i32 numBins,
                           i32 numBins2,
                           f32 threshold) =
  map(fn [int, numBins2] (f32 dot) =>
        loop (dBins = replicate(numBins2, 0)) = for j < numBins do
          if dot > threshold
          then let dBins[numBins+1] = dBins[numBins+1] + 1 in dBins
          else let dBins[numBins] = dBins[numBins] + 1 in dBins
        in dBins
     , points)
