-- Test that intrinsics.opaque prevents constant-folding.
-- ==
-- input {} output {4}
-- structure { BinOp 1 Opaque 1 }

fun main() = intrinsics.opaque 2 + 2
