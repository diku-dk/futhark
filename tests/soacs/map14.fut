-- This program broke the simplifier at one point.

fun main(x: int, y: int, a: []int, b: []int): []int =
  let c = map (\(av: int): (int,int)  ->
                let v = x + y in
                (v, 2*av)) a in
  map (\(x,y)->x+y) c
