-- Entry point result should not be copied.
-- ==
-- structure gpu-mem { SegMap 1 Manifest 0 }
-- structure seq-mem { Manifest 0 }

def main A = flatten A |> map (+2i32) |> unflatten
