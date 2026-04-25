-- Prefix sum that specifically exercises stage-2 loop virtualisation
-- in the two-pass scan, i.e. the case where stage1_num_tblocks >
-- scan_stage2_tblock_size.  The .tuning_gpu file sets
-- scan_stage2_tblock_size to a small value so that even a modest
-- input forces multiple stage-2 iterations.
-- ==
-- input { [1i32, 1i32, 1i32, 1i32, 1i32, 1i32, 1i32, 1i32,
--          1i32, 1i32, 1i32, 1i32, 1i32, 1i32, 1i32, 1i32,
--          1i32, 1i32, 1i32, 1i32, 1i32, 1i32, 1i32, 1i32,
--          1i32, 1i32, 1i32, 1i32, 1i32, 1i32, 1i32, 1i32] }
-- output { [1i32, 2i32, 3i32, 4i32, 5i32, 6i32, 7i32, 8i32,
--           9i32, 10i32, 11i32, 12i32, 13i32, 14i32, 15i32, 16i32,
--           17i32, 18i32, 19i32, 20i32, 21i32, 22i32, 23i32, 24i32,
--           25i32, 26i32, 27i32, 28i32, 29i32, 30i32, 31i32, 32i32] }

def main (a: []i32) : []i32 = scan (+) 0 a
