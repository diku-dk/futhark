-- ==
-- property: prop
#[prop]
entry prop (i: i8) : bool =
  i > 0

-- ==
-- property: prop2
#[prop(shrink(auto))]
entry prop2 (i: []i8) : bool =
    length i < 5

-- ==
-- property: prop3
#[prop(shrink(auto))]
entry prop3 (i: []u64) : bool =
    length i < 5 && u64.sum i < 100

-- ==
-- property: boolArr
#[prop(shrink(auto))]
entry boolArr (xs: []bool): bool =
  and xs


-- n==
-- property: prop4
#[nprop(shrink(auto))]
entry prop4 (i: [][]u8) : bool =
    length i > 5
