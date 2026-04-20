-- ==
-- input { [1,2,3] } output { [4,5,6] }

def A : i32 = #[opaque] 3i32

#[noinline]
def f (x: i32) = x + A

#[noinline]
def g (x: i32) = f x

entry main (xs: []i32) = map g xs
