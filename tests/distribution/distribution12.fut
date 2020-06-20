-- A triply nested map should not cause any multi-versioning.
-- ==
-- structure distributed { SegMap 1 }

let main (xsss: [][][]i32) = map (map (map (+1))) xsss
