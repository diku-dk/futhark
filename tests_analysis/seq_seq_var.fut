def main [n][m] (xss: [n][m]i32) : i32 =
  #[unsafe]
  loop r0=0 for i < n
  do
    r0 + 
    loop r1=0 for j < m
    do
        r1 + xss[i,j]
    

  
-- === Expected output of analysis:
-- TBD