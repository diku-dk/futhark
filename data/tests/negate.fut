// Test that negation works for both integers and reals.
fun {[int],[real]} main([int] a) =
    {map(~, a), map(~, map(toReal, a))}
