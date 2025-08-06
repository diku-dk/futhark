module type bar = {
  type bar 'f
}

module bar_f32 = {
  type bar 'f = f32
}

module type foo = {
  type foo_in
  val foo : foo_in -> f32
}

module foo_f32 = {
  type foo_in = f32
  def foo (x: foo_in) : f32 = x
}

type some_type = i8

module wrapper
  (bar: bar)
  (foo: foo with foo_in = bar.bar some_type) = {
  def baz (x: bar.bar some_type) : f32 = foo.foo x
}

module wrapped = wrapper bar_f32 foo_f32

def main (s: f32) : f32 = wrapped.baz s
