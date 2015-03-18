fun int main([int] a, [int] b) =
    reduce(+, 0, map(*, zip(a,b)))
