fun [[real]] main(int dimX, [[real]] X) = 
    map( fn [real] ([real] myResult) =>
        map( fn real (int i) => 
            let res1 = if ( i < 1 || myResult[i] == 0.0 )
                       then 0.0
                       else 0.5 * myResult[i-1]  in
            let res2 = if ( i <= dimX - 2 &&  0.0 < myResult[i]) 
                       then 0.5 * myResult[i+1]
                       else 0.0
            in res1 + res2 + 0.5 * myResult[i]
        , iota(dimX) )
    , X )
// i-1 < sizeX, i \in [1, dimX-1];  i+1 < sizeX if i \in [0,dimX-2]
