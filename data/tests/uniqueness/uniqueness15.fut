-- At one point this program was invalidly transformed, leading to a
-- uniqueness error.
--
-- This was due to inconsistencies in how consumption was handled in
-- the core language.
--
-- From issue #191.

fun *[i32, numBins] reduceBins(*[i32,numBins] acc, *[i32,numBins] elm) =
    loop (newVal = acc) = for i < numBins do
        let newVal[i] = newVal[i] + elm[i] in newVal
    in
        newVal

fun [i32] main() =
    let
        (RRs, DRs) = unzip(map(fn (*[i32], *[i32]) (i32 i) => (replicate(2, 0), replicate(2, 0)), iota(1)))
    in
    loop ((res, RR, DR) = (replicate(2, 0),
                           reduce(reduceBins, replicate(3, 0), RRs),
                           reduce(reduceBins, replicate(3, 0), DRs)
                          )) = for i < 1 do
        let res[i*2] = DR[i+1] in
        let res[i*2+1] = RR[i+1] in
        (res, RR, DR)
    in
    res
