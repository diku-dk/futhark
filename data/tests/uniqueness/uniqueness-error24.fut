// Test that consumption checking is done even with no meaningful
// bindings.

fun int consume(*[int] a) = 0 // OK.

fun [int] main(*[int] a) =
  let _ = consume(a) in
  a // Should fail, because a has been consumed!
