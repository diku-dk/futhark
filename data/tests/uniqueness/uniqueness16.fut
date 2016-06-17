-- From issue #191.
-- ==
-- input { 1 }
-- output { [0i64, 0i64] }

default(f32)

fun [num]f32 iota32(i32 num) =
    map(f32, iota(num))

fun *[numBins]i64 reduceBins(*[numBins]i64 acc, *[numBins]i64 elm) =
    loop (newVal = acc) = for i < numBins do
        let newVal[i] = newVal[i] + elm[i] in newVal
    in
        newVal

fun *[numBins2]i64 doCompute(
    [num1]f32 data1,
    [num2]f32 data2,
    i32 numBins,
    i32 numBins2,
    [numBBins]f32 bin
) =
    let value = map(fn *[numBins2]i64 (f32 x) =>
            let vals = map(fn *[numBins2]i64 (f32 y) =>
                    let dot = x*y in
                    let dBins = replicate(numBins2, 0i64) in
                    let dBins[0] = 1i64 in dBins
                , data2)
            in
                reduce(reduceBins, replicate(numBins2, 0i64), vals)
        , data1)
    in
    reduce(reduceBins, replicate(numBins2, 0i64), value)

fun *[]i64 main(int numBins) =
    let binb = map(fn f32 (f32 k) => k, iota32(numBins + 1))
    let datapoints = iota32(10)
    let randompoints = replicate(1, datapoints)
    let (rrs, drs) = unzip(map(fn (*[]i64, *[]i64) ([numR]f32 random) => (replicate(numBins+2, 0i64), doCompute(datapoints, random, numBins, numBins+2, binb)), randompoints))
    in
    loop ((res, rr, dr) = (replicate(numBins*2, 0i64),
                               reduce(reduceBins, replicate(numBins+2, 0i64), rrs),
                               reduce(reduceBins, replicate(numBins+2, 0i64), drs)
                               )) = for i < numBins do
        let res[i*2] = dr[i+1] in
        let res[i*2+1] = rr[i+1] in
        (res, rr, dr)
    in
    res
