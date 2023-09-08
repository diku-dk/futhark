def main [n][m] (xss: [n][m]i32) : i32 =
  #[unsafe]
  loop r0=0 for i < n
  do
    r0 + 
    loop r1=0 for j < m
    do
        r1 + xss[i,j]
    

  
-- === Expected output of analysis:
-- entry_main
--   main_res_5104 => [
--     main_res_dev_5113
--       [ σ xss_5346 | ν seq ]  [ σ xss_5346 | ν seq ]
--   ]
--   const_dev_5114 => []

-- WARNING: this output might be wrong!!!

