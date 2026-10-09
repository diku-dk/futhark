-- Can we read our own source code?
-- Handle the fact that a text file in a windows checkout can have a longer
-- byte length because of automatic CRLF conversions
-- ==
-- script input { io.loadbytes "script6.fut" }
-- output { 310i64 }

def main (s: []u8) = i64.sum (map (\x -> if x == 13 then 0 else 1) s)
