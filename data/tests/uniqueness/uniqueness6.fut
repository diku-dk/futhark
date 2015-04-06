// --
// input {
//   [1,2,3]
// }
// output {
//   [1,2,3]
// }
fun [  int  ] main( [  int  ]  arr) =
  let  newarr  =
       (let notused  = arr in
        let n = size(0, arr) in
        copy(replicate(n , 0))) in
  let newarr[0] = 0 in
  arr
