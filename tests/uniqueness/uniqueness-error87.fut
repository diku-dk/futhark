-- The result of a function in a module that may return a global array
-- cannot be consumed.
-- ==
-- error: "tl", which is not consumable

module M = {
  def global : [3]i32 = [1, 2, 3]
  def tl (_: i32) : []i32 = global[1:]
}

def main (x: i32) = M.tl x with [0] = 1
