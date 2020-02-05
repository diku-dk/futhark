let expand_with_flags [n] 'a 'b
                      (b: b)
                      (sz: a -> i32) (get: a -> i32 -> b)
                      (arr:[n]a) : ([]b, []bool)  =
  ([], [])

type^ csr 't = {row_off: []i32, col_idx: []i32, vals: []t}

let expand_reduce 'a 'b [n]
                  (sz: a -> i32) (get: a -> i32 -> b)
                  (f: b -> b -> b) (ne:b)
                  (arr:[n]a) : []b =
  let (vals, flags) = expand_with_flags ne sz get arr
  in []

let smvm ({row_off,col_idx,vals} : csr i32) (v:[]i32) =
  let rows = map (\i -> (i,
			 row_off[i],
			 row_off[i+1]-row_off[i]))
                 (iota(length row_off - 1))
  let sz r = r.2
  let get r i = vals[r.1+i] * v[col_idx[r.1+i]]
  in expand_reduce sz get (+) 0 rows

let m_csr : csr i32 =
  {row_off=[0,3,5,8,9,11],          -- size 6
   col_idx=[0,1,3,1,2,1,2,3,3,3,4], -- size 11
   vals=[1,2,11,3,4,5,6,7,8,9,10]}  -- size 11

let v : []i32 = [3,1,2,6,5]

let main (_ : i32) : []i32 =
  smvm m_csr (copy v)
