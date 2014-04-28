// Test that negation works for both integers and reals.
fun {[int],[real]} main([int] a) =
    {map(op ~, a), map(op ~, map(toReal, a))}
