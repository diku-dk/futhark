-- Migration analysis is also done for top-level constants.
--
-- Only constants that are used by functions need to be read to host.
-- ==
-- structure gpu {
--   GPUBody 0
--   /Index 2
-- }

def arr = opaque [4i32, 2i32]
def a = arr[0]
def b = arr[1]
def c = a + b

def main (x: i32) : bool =
  c == x || b == x
