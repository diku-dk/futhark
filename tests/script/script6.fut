-- Can we read our own source code?
-- ==
-- script input { $loadbytes "script6.fut" }
-- output { 139i64 }

def main (s: []u8) = length s
