fun [[real]] main(int dimX, [[real]] X) = 
    map( fn [real] ([real] myResult) =>
        map( fn real (int i) => 
//              myResult[i]
            let res1 = if ( -i <= -1  )
                       then 0.5 * myResult[i-1]
                       else 0.0  in
//            let res2 = if(-dimX + 2 <= -i) 
//                       then 0.5 * myResult[i+1]
//                       else 0.0
//            in res1 + res2 //+ myResult[i]
           res1
        , iota(dimX) )
    , X )

