-- The problem is that a simplified index function is used.

let main (b: bool) (xs: []i32) =
  map (\(x: i32) -> if b
             then (rearrange (1,0) [[x,x],[x,x]]) with [0,0] <- 7i32
             else [[1,1],[1,1]]) xs