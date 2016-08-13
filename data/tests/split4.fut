-- A better name would be 'take'...
-- ==
-- input { 2 [1,2,3,4,5] }
-- output { [1,2] }
fun []int main(int n, []int a) =
  split( (n), a).0
