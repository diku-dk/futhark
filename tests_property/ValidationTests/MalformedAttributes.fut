-- MalformedAttributeTests.fut
--
-- This file checks that malformed or misspelled property attributes are reported
-- as validation errors, rather than being silently ignored.

-- ------------------------------------------------------------
-- Shared helper entry points
-- ------------------------------------------------------------

entry gen_i32 (_size: i64) (_seed: i32) : i32 =
  1i32

entry shrink_i32 (x: i32) (_random: i32) : i32 =
  x / 2i32

entry print_i32 (x: i32) : []u8 =
  if x == 0i32 then "0" else "nonzero"


-- ------------------------------------------------------------
-- 1. Misspelled outer property attribute: propp instead of prop
-- ------------------------------------------------------------

-- ==
-- property: prop_misspelled_outer_attribute

#[prop]
entry prop_misspelled_outer_attribute (x: i32) : bool =
  x == x


-- ------------------------------------------------------------
-- 2. Misspelled generator field: genn instead of gen
-- ------------------------------------------------------------

-- ==
-- property: prop_misspelled_gen_field

#[prop(genn(gen_i32))]
entry prop_misspelled_gen_field (x: i32) : bool =
  x == x


-- ------------------------------------------------------------
-- 3. Misspelled shrinker field: shrnk instead of shrink
-- ------------------------------------------------------------

-- ==
-- property: prop_misspelled_shrink_field

#[prop(gen(gen_i32), shrnk(shrink_i32))]
entry prop_misspelled_shrink_field (x: i32) : bool =
  x == 0i32


-- ------------------------------------------------------------
-- 4. Misspelled pretty-printer field: ppprint instead of pprint
-- ------------------------------------------------------------

-- ==
-- property: prop_misspelled_pprint_field

#[prop(gen(gen_i32), ppprint(print_i32))]
entry prop_misspelled_pprint_field (x: i32) : bool =
  x == 0i32


-- ------------------------------------------------------------
-- 5. Misspelled size field: sze instead of size
-- ------------------------------------------------------------

-- ==
-- property: prop_misspelled_size_field

#[prop(gen(gen_i32), sze(10))]
entry prop_misspelled_size_field (x: i32) : bool =
  x == x