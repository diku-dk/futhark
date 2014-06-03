// Simplest interesteing loop - factorial function.
fun int main(int n) =
  loop (x = 1) = for i < n do
    x * (i + 1)
  in x
