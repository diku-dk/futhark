type t [n] = #foo ([n]i32)
           | #bar ([n]i32) ([n]i32)

let main [n] (x: t [n]) = x
