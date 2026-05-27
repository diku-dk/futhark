entry gen (size: i64) (_seed: u64) : i64 = size

-- ==
-- property: prop

#[prop(gen(gen))]
entry prop (x: i64) : bool = x >= 0
