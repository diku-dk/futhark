-- At one point this program was invalidly transformed, leading to a
-- uniqueness error.
--
-- This was due to inconsistencies in how consumption was handled in
-- the core language.
--
-- From issue #191.

fun reduceBins(acc: *[numBins]i32) (elm: *[numBins]i32): *[numBins]i32 =
    loop (newVal = acc) = for i < numBins do
        let newVal[i] = newVal[i] + elm[i] in newVal
    in
        newVal

fun main(): []i32 =
    let
    (rrs, drs) = unzip(map (fn (i: i32): (*[]i32, *[]i32) =>
                             (replicate 2 0,
                              replicate 2 0)) (
                           iota 1))
    in
    loop ((res, rr, dr) = (replicate 2 0,
                           reduce reduceBins (replicate 3 0) rrs,
                           reduce reduceBins (replicate 3 0) drs
                          )) = for i < 1 do
        let res[i*2] = dr[i+1]
        let res[i*2+1] = rr[i+1] in
        (res, rr, dr)
    in
    res
