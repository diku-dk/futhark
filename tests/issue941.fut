type sometype 't = #someval t

let geni32 (maxsize : i32) : sometype i32 = #someval maxsize

let genarr 'elm
           (genelm: i32 -> sometype elm)
           (ownsize : i32)
           : sometype ([ownsize](sometype elm)) =
  #someval (tabulate ownsize genelm)

let main = genarr geni32 1
