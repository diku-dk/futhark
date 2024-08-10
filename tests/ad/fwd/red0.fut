def f x = reduce (*) 1 [1,2,x,4]

-- ==
<<<<<<< HEAD
-- entry: f_jvp 
-- input { 3 } output { 8 }
-- input { 10 } output { 8 }
=======
-- entry: f_jvp
-- compiled input { 3 } output { 8 }
-- compiled input { 10 } output { 8 }
>>>>>>> master
entry f_jvp x = jvp f x 1
