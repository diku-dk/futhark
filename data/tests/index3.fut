// Test indexing with an index that is the result of a branch.  The
// intent here is to make sure that the bounds check can still be
// optimised away.

fun int main(bool b) =
  let a = [1,2,3] in
  let i = if b then 0 else 1 in
  a[i]
