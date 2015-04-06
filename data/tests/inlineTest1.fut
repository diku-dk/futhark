// --
// input {
//   42
//   1337
// }
// output {
//   24730855
// }
fun int fun1(int a, int b) = a + b

fun int fun2(int a, int b) = fun1(a,b) * (a+b)

fun int fun3(int a, int b) = fun2(a,b) + a + b

fun int main(int n, int m) =
  fun1(n,m) + fun2(n+n,m+m) + fun3(3*n,3*m) + fun2(2,n) + fun3(n,3)
