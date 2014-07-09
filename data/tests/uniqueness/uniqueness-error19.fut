// Test that you cannot consume free variables in a loop.

fun int main() =
  let n = 10 in
  let a = copy(iota(n)) in
  let b = copy(iota(n)) in
  loop (b) = for i < n do
               let a[i] = i in // Error, because a is free and
                               // should not be consumed.
               b in
  0
