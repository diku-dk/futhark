-- Test some bit operations in parallel context (e.g. on GPU).  It is
-- assumed that the tests in primitive/ validate the sequential
-- reference results.
-- ==
-- compiled random input { [100]u8 [100]u16 [100]u32 [100]u64 } auto output

def main (u8s: []u8) (u16s: []u16) (u32s: []u32) (u64s: []u64) =
  ( map u8.popc u8s
  , map u16.popc u16s
  , map u32.popc u32s
  , map u64.popc u64s
  , map u8.clz u8s
  , map u16.clz u16s
  , map u32.clz u32s
  , map u64.clz u64s
  )
