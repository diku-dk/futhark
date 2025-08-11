-- Size variables must be made available on host before use and thus block the
-- migration of any parent statements.
-- ==
-- structure gpu {
--   /If 1
--   /If/True/If 1
--   /If/True/If/True/GPUBody/If 1
--   /If/True/GPUBody/If 1
-- }

def main (A: [5]i64) : [1]i64 =
  if A[0] == 0
  then -- blocked.
       let x =
         if A[1] == 0
         then -- blocked.
              let n = A[2]
              -- required on host.
              in if A[3] == 0
                 then -- not blocked.
                      let B = A[:n]
                      -- n used as a size variable.
                      in opaque B :> [1]i64
                 else let i = n % 5
                      in A[i:i + 1] :> [1]i64
         else A[0:1] :> [1]i64
       in if A[4] == x[0]
          then -- not blocked.
               A[1:2] :> [1]i64
          else A[2:3] :> [1]i64
  else A[3:4] :> [1]i64
