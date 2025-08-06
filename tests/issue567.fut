-- Infinite loops should not crash the compiler.

def main (x: i32) = loop x while true do x + 1
