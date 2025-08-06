-- A lambda function must not return a global array.
-- ==
-- error: aliases the free variable "global"

def global : []i32 = [1, 2, 3]

def main = \(b: bool) -> if b then global else []
