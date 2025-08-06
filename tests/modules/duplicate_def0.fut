-- This test is written to ensure that the same name can be used
-- for different declarations in the same local environment, as long as their types does not overlap.
-- ==
-- input { 4 }
-- output { 4 }

type foo = i32
def foo (a: i32) : foo = a + a

module Foo = {
  def one () : i32 = 1
}

def main (x: i32) : i32 = x
