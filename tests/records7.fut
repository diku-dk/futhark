-- Even a large tuple works as a record.
--
-- ==
-- input { 0 } output { 5 }

let main(x: i32) =
  let (a,b,c,d,e,f,g,h,i,j,k,l,m,n) =
    {1=x+1, 2=x+2, 3=x+3, 4=x+4, 5=x+5,
     6=x+6, 7=x+7, 8=x+8, 9=x+9, 10=x+10,
     11=x+11, 12=x+12, 13=x+13, 14=x+14}
  in e
