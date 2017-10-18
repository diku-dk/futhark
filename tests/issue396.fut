-- The problem is that a simplified index function is used.

let main (b: bool) (xs: []i32) =
  map (\x -> if b
             then (reshape (2,2) [[x,x],[x,x]]) with [0,0] <- 7
             else [[1,1],[1,1]]) xs