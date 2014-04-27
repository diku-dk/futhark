fun [real] main([{real,int}] a, [bool] oks) =
  let {b, _} = unzip(filter(fn bool ({real,int} x) =>
                              let {_,i} = x in
                              oks[i],
                            a)) in
  b
