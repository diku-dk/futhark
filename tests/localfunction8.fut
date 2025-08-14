-- It is OK for a local function to alias a local array.
-- ==
-- input { true } output { [1,2,3] }
-- input { false } output { empty([0]i32) }

def main b =
  let global: []i32 = [1, 2, 3]
  let f (b: bool) = if b then global else []
  in f b
