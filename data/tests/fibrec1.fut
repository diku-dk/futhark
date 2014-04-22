fun int fib(int n) = if n < 2 then 1 else fib(n-1) + fib(n-2)

fun int main(int n) = fib(n)
