fun [int] main([int] a) =
  let {_,b} = unzip(map(fn {int,int} (int x) => {x+2,x-2}, a)) in
  let c = scan(+, 0, b) in
  c
