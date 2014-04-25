// Test that negation works for both integers and reals.
fun {[int],[real]} main() =
    {map(op ~, iota(3)), map(op ~, map(toReal, iota(3)))}
