-- A loop may run zero times, in which case its result is its initial value.  So
-- the lifted function for "iterate" must not be declared to return a freshly
-- constructed value merely because its body ("copy x") does - the result may be
-- the parameter "x" instead.  See Note [Dynamic diets] in
-- Futhark.Internalise.Defunctionalise.
-- ==
-- input { 0i32 [1,2,3] }
-- output { [1,2,3] }
-- input { 3i32 [1,2,3] }
-- output { [1,2,3] }

def main (n: i32) (xs: []i32) : []i32 = iterate n copy xs
