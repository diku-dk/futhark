-- ==
-- error: Default

module type Addable = {
	type t
	val add: t -> t -> t
}
module Add_f32:Addable = {
	type t = f32
	let add a b = a + b
}
