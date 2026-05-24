-- Entry point result should not be copied.
--
-- ...for the CPU backend, it is currently (potentially) copied because of a
-- conservative assumption when inserting allocations, that we do not optimise
-- away properly later. That's not terribly important right now, but should be
-- fixed some day.
-- ==
-- structure gpu-mem { SegMap 1 Manifest 0 EnsureDirect 0 }
-- structure seq-mem { EnsureDirect 1 }

def main A = flatten A |> map (+ 2i32) |> unflatten
