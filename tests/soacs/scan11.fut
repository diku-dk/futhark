-- Prefix sum exercising stage-2 loop virtualisation in the two-pass
-- scan with a large input.  Tuning sets scan_stage2_tblock_size=4 and
-- segscan_num_tblocks=16 so stage-2 needs ceil(16/4)=4 iterations.
-- Large input is excluded from oclgrind (compiled tag).
--
-- Only OpenCL uses the two-pass scan; CUDA and HIP use single-pass.
-- ==
-- tags { no_cuda no_hip }
-- compiled random input { [1000000]i32 } auto output

def main (a: []i32) : []i32 = scan (+) 0 a
