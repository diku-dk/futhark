-- This program broke the simplifier at one point.

fun main(x: int, y: int, a: []int, b: []int): []int =
  let c = map (fn (av: int): (int,int)  =>
                let v = x + y in
                (v, 2*av)) a in
  map (fn(x,y)=>x+y) c
