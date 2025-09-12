--
-- ==
-- tags { autodiff }
-- entry: rev
-- input { [[1.0,2.0,3.0,4.0],[1.0,2.0,3.0,4.0]] [1.0,2.0] }
-- output {[[24.0, 12.0, 8.0, 6.0],
--          [48.0, 24.0, 16.0, 12.0]] }

entry rev = vjp (map f64.product)
