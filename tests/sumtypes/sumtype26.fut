-- Issue 785

type mbpd = #Just {pos:i32}

let main (pd: mbpd) =
  match pd
  case #Just {pos} -> pos
