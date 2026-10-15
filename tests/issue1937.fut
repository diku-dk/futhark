-- A global array with size parameters may be (a slice of) another global
-- array, so its result cannot be fresh.
-- ==
-- error: "iiota", which is not consumable

def iiota [n] : [n]i64 = 0..1..<n

def main n : *[n]i64 = iiota
