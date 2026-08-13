-- Updating with a slice that covers the entire array, but reorders its
-- elements, must not be simplified into a plain copy.
-- ==
-- input { [1,2,3,4,5] [10,20,30,40,50] }
-- output { [50,40,30,20,10] }

entry main (xs: *[5]i32) (ys: [5]i32) = xs with [4:-1:-1] = ys
