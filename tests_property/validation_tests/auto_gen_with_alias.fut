type b = bool
type t = (b, b)
type~ arr = []t
type~ tt = (arr, arr)

-- ==
-- property: primitive_alias
-- property: tuple_alias
-- property: array_alias
-- property: nested_tuple_array_alias

#[prop]
entry primitive_alias (x: b) : bool = x

#[prop]
entry tuple_alias (x: t) : bool = x.0 && x.1

#[prop]
entry array_alias (x: arr) : bool =
  let (x,y) = unzip x in
  and x && and y

#[prop]
entry nested_tuple_array_alias (x: tt) : bool =
  let (arr1, arr2) = x in
  let (x1,y1) = unzip arr1 in
  let (x2,y2) = unzip arr2 in
  and x1 && and y1 && and x2 && and y2
