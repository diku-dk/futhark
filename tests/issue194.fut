-- Test program for issue #194.
--
-- Revealed a bug in in-place-lowering.
-- ==

let main [numD]
        (points: [numD]f32)
        (numBins: i32)
        (numBins2: i32)
        (threshold: f32): [][numBins2]i32 =
  map (\(dot: f32): [numBins2]i32  ->
        loop dBins = replicate numBins2 0 for j < numBins do
          if dot > threshold
          then unsafe let dBins[numBins+1] = dBins[numBins+1] + 1 in dBins
          else unsafe let dBins[numBins] = dBins[numBins] + 1 in dBins
     ) points
