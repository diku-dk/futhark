fun {int,[int]} main(int m, *[int,n] A) = 
  stream( fn {int,*[int]} (int acc, *[int] C) =>
                    let W = filter( >6, C ) in
                    { acc, W }
        , chunk, i, 0, A
        )

        
