-- Reproducer for wrong result from noncommutative segmented reduction on GPU backends.
-- ==
-- compiled random input { [1][256]i32 } auto output
-- compiled random input { [100][256]i32 } auto output
-- compiled random input { [100][512]i32 } auto output

def main xs = #[incremental_flattening(only_inner)] map (reduce (\x y -> opaque x + y) 0i32) xs
