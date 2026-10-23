-- More fancy use of module-level lambdas.
-- ==
-- input { 9 } output { 3 }

module type operation = {type a type b val f : a -> b}

module compose = \(F: operation) ->
  \(G: operation with a = F.b) ->
    {
      type a = F.a
      type b = G.b

      def f (x: a) = G.f (F.f x)
    }

module i32_to_f32 : operation with a = i32 with b = f32 = {
  type a = i32
  type b = f32
  def f (x: a) = f32.i32 x
}

module f32_to_i32 : operation with a = f32 with b = i32 = {
  type a = f32
  type b = i32
  def f (x: a) = i32.f32 x
}

module f32_sqrt : operation with a = f32 with b = f32 = {
  type a = f32
  type b = f32
  def f (x: a) = f32.sqrt x
}

module i32_sqrt = compose (compose i32_to_f32 f32_sqrt) f32_to_i32

def main (x: i32) = i32_sqrt.f x
