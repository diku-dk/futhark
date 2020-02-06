-- Standard polymorphic function composition.
-- ==
-- input { 5 } output { true [6,6,6] [false,false,false] }
-- input { 2 } output { false [3,3,3] [true,true,true] }

let compose 'a 'b 'c (f : b -> c) (g : a -> b) (x : a) : c = f (g x)

let add1 (x : i32) : i32 = x+1
let isEven (x : i32) : bool = x % 2 == 0
let replicate3 'a (x : a) : [3]a = [x, x, x]

let main (x : i32) =
  (compose isEven add1 x,
   compose replicate3 add1 x,
   compose replicate3 isEven x
)
