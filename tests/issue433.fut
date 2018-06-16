-- The bug here was related to the replicate.

let main () = replicate 0 ([] : [](i32,i32))
