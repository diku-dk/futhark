-- Prefix sum exercising stage-2 loop virtualisation in the two-pass
-- scan with a larger thread block size.  Tuning sets
-- scan_stage2_tblock_size=8 and segscan_num_tblocks=32 so stage-2
-- needs ceil(32/8)=4 iterations.  Large input is excluded from
-- oclgrind (compiled tag).
--
-- Only OpenCL uses the two-pass scan; CUDA and HIP use single-pass.
-- ==
-- tags { no_cuda no_hip }
-- compiled random input { [1000000]i32 } auto output

def main (a: []i32) : []i32 = scan (+) 0 a
