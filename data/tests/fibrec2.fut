fun *[int] fib(*[int] a, int i, int n) =
  if i == n
  then a
  else if i < 2 then let a[i] = 1 in fib(a,i+1,n)
                else let a[i] = a[i-1]+a[i-2] in fib(a,i+1,n)

fun [int] main(int n) = fib(copy(replicate(n,0)),0,n)
