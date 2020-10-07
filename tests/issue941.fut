type sometype 't = #someval t

let geni32 (maxsize : i64) : sometype i64 = #someval maxsize

let genarr 'elm
           (genelm: i64 -> sometype elm)
           (ownsize : i64)
           : sometype ([ownsize](sometype elm)) =
  #someval (tabulate ownsize genelm)

let main = genarr geni32 1
