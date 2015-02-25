fun [[[int]]] main () =
  let xss = iota(16) in
  let tmp = reshape( (2,4,2), xss ) in
  rearrange( (2,0,1), tmp )
