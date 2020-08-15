let main [n] (as:[n]i32) (bs:[n]i32) =
	let indices = iota n
	let sas = scan (+) 0 as
	let false_inds =  map3 (\a b i-> if a == b then -1 else i) sas bs indices
	let list_of_false = filter (>=0) false_inds
	let result = if (length list_of_false == 0) 
		     then 	(-1,-1,-1)
		     else	let i = head list_of_false 
				in ( i, sas[i], bs[i])
	in result

