-- More fancy use of module-level lambdas.
-- ==
-- input { 9 } output { 3 }
module type operation = {type a type b val f: a -> b}

module compose = \(F: operation) ->
\(G: operationwith a  = F.b) ->
{
  type a = F.a
  
  type b = G.b
  
  def f (x: a) = G.f (F.f x)
}

module i32_to_f64: operationwith a  = i32with b  = f64 = {
  type a = i32
  
  type b = f64
  
  def f (x: a) = f64.i32 x
}

module f64_to_i32: operationwith a  = f64with b  = i32 = {
  type a = f64
  
  type b = i32
  
  def f (x: a) = i32.f64 x
}

module f64_sqrt: operationwith a  = f64with b  = f64 = {
  type a = f64
  
  type b = f64
  
  def f (x: a) = f64.sqrt x
}

module i32_sqrt = compose {compose i32_to_f64 f64_sqrt} f64_to_i32

def main (x: i32) = i32_sqrt.f x