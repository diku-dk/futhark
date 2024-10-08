-- Module-level lambdas, i.e., anonymous functors.
-- ==
-- input { 2 } output { 32 }

module type operation = { type t val f: t -> t }

module type repeater = (P:operation) -> operation with t = P.t

module twice: repeater = \(P: operation) -> {
  type t = P.t
  def f (x: P.t) = P.f (P.f x)
}

module type i32_operation = operation with t = i32

module times_2: i32_operation = { type t = i32 def f (x: i32) = x * 2 }
module times_4: i32_operation = twice(times_2)
module times_16: i32_operation = twice(times_4)

def main (x: i32) = times_16.f x
