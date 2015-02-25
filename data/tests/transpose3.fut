fun [[[int]]] main () =
  let xss = iota(16) in
  let tmp = reshape( (2,4,2), xss ) in
  transpose(0,2, tmp)
