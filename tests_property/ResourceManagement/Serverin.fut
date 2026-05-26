-- ==
-- property: prop_auto_shrink_reuses_putval_names

-- This test is not about the Futhark property itself.
-- It is an implementation test for the runner.
--
-- The auto-shrinker repeatedly calls the generator with smaller sizes.
-- This forces the runner to repeatedly write qc_size and qc_seed with putVal.
-- If putVal does not correctly handle reused server variable names, this test
-- may fail with a variable/name-collision error.

entry gen_array (size: i64) (seed: u64) : []i32 =
  if size < 0 
    then [] 
    else replicate size (i32.u64 seed)

#[prop(gen(gen_array),size(10))]
entry prop_auto_shrink_reuses_putval_names (_xs: []i32) : bool =
  false
