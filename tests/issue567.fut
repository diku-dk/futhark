-- Infinite loops should not crash the compiler.

let main (x: i32) = loop x while true do x+1
