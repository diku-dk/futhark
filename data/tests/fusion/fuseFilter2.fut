// --
fun int main([int] a, [int] b) =
  let {a2,b2} = unzip(filter(<, zip(a,b))) in
  reduce(+, 0, b2)
