-- A triply nested map should not cause any multi-versioning.
-- ==
-- structure gpu { SegMap 1 }

let main (xsss: [][][]i32) = map (map (map (+1))) xsss
