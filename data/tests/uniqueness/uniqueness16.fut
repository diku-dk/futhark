-- From issue #191.
-- ==
-- input { 1 }
-- output { [0i64, 0i64] }

default(f32)

fun iota32(num: i32): [num]f32 =
    map f32 (iota(num))

fun reduceBins(acc: *[numBins]i64) (elm: *[numBins]i64): *[numBins]i64 =
    loop (newVal = acc) = for i < numBins do
        let newVal[i] = newVal[i] + elm[i] in newVal
    in
        newVal

fun doCompute(data1:
    [num1]f32,
    data2: [num2]f32,
    numBins: i32,
    numBins2: i32,
    bin: [numBBins]f32
): *[numBins2]i64 =
    let value = map (fn (x: f32): *[numBins2]i64  =>
            let vals = map (fn (y: f32): *[numBins2]i64  =>
                    let dot = x*y
                    let dBins = replicate numBins2 0i64
                    let dBins[0] = 1i64 in dBins
                ) data2
            in
                reduce reduceBins (replicate numBins2 0i64) vals
        ) data1
    in
    reduce reduceBins (replicate numBins2 0i64) value

fun main(numBins: int): *[]i64 =
    let binb = map (fn (k: f32): f32  => k) (iota32(numBins + 1))
    let datapoints = iota32(10)
    let randompoints = replicate 1 datapoints
    let (rrs, drs) = unzip(map (fn (random: [numR]f32): (*[]i64, *[]i64)  =>
                                 (replicate (numBins+2) 0i64,
                                  doCompute(datapoints, random, numBins, numBins+2, binb))) randompoints)
    in
    loop ((res, rr, dr) =
          (replicate (numBins*2) 0i64,
           reduce reduceBins (replicate (numBins+2) 0i64) rrs,
           reduce reduceBins (replicate (numBins+2) 0i64) drs)) = for i < numBins do
      let res[i*2] = dr[i+1]
      let res[i*2+1] = rr[i+1] in
      (res, rr, dr)
    in res
