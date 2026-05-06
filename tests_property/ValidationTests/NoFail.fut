entry gen (size : i64) (_seed : i32) : i64 = size


-- ==
-- property: prop
#[prop(gen(gen), shrink(auto))]
entry prop (x : i64) : bool = x >= 0

