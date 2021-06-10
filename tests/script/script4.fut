-- ==
-- entry: doeswork
-- script input { let x = 2 in let y = 3 in mkdata x y } output { 10 }

entry mkdata x y = x + y : i32

entry doeswork z = z * 2
