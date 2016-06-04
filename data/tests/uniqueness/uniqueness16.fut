-- From issue #191.
-- ==
-- input { 1 }
-- output { [0i64, 0i64] }

default(f32)

fun [f32, num] iota32(i32 num) =
    map(f32, iota(num))

fun *[i64, numBins] reduceBins(*[i64,numBins] acc, *[i64,numBins] elm) =
    loop (newVal = acc) = for i < numBins do
        let newVal[i] = newVal[i] + elm[i] in newVal
    in
        newVal

fun *[i64, numBins2] doCompute(
    [f32, num1] data1,
    [f32, num2] data2,
    i32 numBins,
    i32 numBins2,
    [f32, numBBins] bin
) =
    let value = map(fn *[i64, numBins2] (f32 x) =>
            let vals = map(fn *[i64, numBins2] (f32 y) =>
                    let dot = x*y in
                    let dBins = replicate(numBins2, 0i64) in
                    let dBins[0] = 1i64 in dBins
                , data2)
            in
                reduce(reduceBins, replicate(numBins2, 0i64), vals)
        , data1)
    in
    reduce(reduceBins, replicate(numBins2, 0i64), value)

fun *[i64] main(int numBins) =
    let binb = map(fn f32 (f32 k) => k, iota32(numBins + 1))
    let Datapoints = iota32(10)
    let Randompoints = replicate(1, Datapoints)
    let (RRs, DRs) = unzip(map(fn (*[i64], *[i64]) ([f32, numR] random) => (replicate(numBins+2, 0i64), doCompute(Datapoints, random, numBins, numBins+2, binb)), Randompoints))
    in
    loop ((res, RR, DR) = (replicate(numBins*2, 0i64),
                               reduce(reduceBins, replicate(numBins+2, 0i64), RRs),
                               reduce(reduceBins, replicate(numBins+2, 0i64), DRs)
                               )) = for i < numBins do
        let res[i*2] = DR[i+1] in
        let res[i*2+1] = RR[i+1] in
        (res, RR, DR)
    in
    res
