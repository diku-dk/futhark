----------------------------------------------------
--- lifted version of partition2
----------------------------------------------------
let partition2L 't [n] [m]
                -- the shape of condsL is also shp
                (condsL: [n]bool) (dummy: t)
                (shp: [m]i64, arr: [n]t) :
                ([m]i64, [n]t) =
  let begs   = scan (+) 0 shp --- prepend 0 here; prepend 0 to shp
  let flags  =  (  iota m
                |> map (+1)
                |> mkFlagArray 0i64 shp
                ) :> [n]i64

  let outinds= map (\f -> if f==0 then 0 else f-1) flags
            |> sgmSumInt flags

  let tflgsL = map (\c -> if c then 1i64 else 0i64) condsL
  let fflgsL = map (\b -> 1 - b) tflgsL

  let indsTL = sgmSumInt flags tflgsL
  let tmpL   = sgmSumInt flags fflgsL

  -- let lst = indsT[n-1]
  let lstL   = map2 (\s b -> if s==0 then -1 else #[unsafe] indsTL[b-1]
                    ) shp begs

  -- let indsF = map (+lst) tmp
  let indsFL = map2 (\t sgmind-> t + #[unsafe] lstL[sgmind]) tmpL outinds

  let indsL = map4(\c indT indF sgmind->
                        let offs = if sgmind > 0 then #[unsafe] begs[sgmind-1] else 0i64
                        in  if c then offs + indT - 1
                                 else offs + indF - 1
                  ) condsL indsTL indsFL outinds

  let fltarrL = scatter (replicate n dummy) indsL arr
  in  (lstL, fltarrL)
