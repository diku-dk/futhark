-- A functor that swaps types
-- ==
-- input {}
-- output { 3 }
module F (X: {type t type s}): {type t = X.s type s = X.t} = {type t = X.s type s = X.t}

module A = {type t = f32 type s = i32}

module B = F {A}

module C = F {B}

def main: i32 = 3