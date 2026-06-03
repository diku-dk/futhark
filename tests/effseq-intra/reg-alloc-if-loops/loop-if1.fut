let imap  as f = map f as
let imap2 as bs f = map2 f as bs
let imap2Intra as bs f = #[incremental_flattening(only_intra)] map2 f as bs

def pairAdd [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  let rs = #[scratch] replicate n 0
  in  loop rs for i < n do
        let rs[i] = as[i] + bs[i]
        in  rs

def pairAddMap [n] (as: [n]u64) (bs: [n]u64) : [n]u64 =
  #[sequential] map2 (+) as bs

let badd [ipb][n] (q:i32) (as : [(ipb*n)][4]u64) (bs : [(ipb*n)][4]u64) : [(ipb*n)*4]u64 =
  let ash = #[glb2reg_only(1)] manifest as
  let bsh = #[glb2reg_only(1)] manifest bs

  let rsh =
    loop rsh = ash for i < q do
      let tmp =
          match i % 2
          case 0 -> map2 pairAddMap rsh ash
          case _ -> map2 pairAddMap rsh bsh
          -- BUG: when removing the #[toregmem(1)] it does not validate anymore! 
      let tmp'= #[inform_pardim_only(1)] manifest tmp
      let res =
          if (i % 3 == 0)
          then map2 pairAddMap tmp' bsh
          else map2 pairAddMap tmp' ash  -- #[toregmem(1)] 
      in  #[inform_pardim_only(1)] manifest res

  in  flatten rsh

-- ==
-- entry: oneAddition1024
-- compiled random input { 100i32 [32768][512][4]u64  [32768][512][4]u64 } auto output
entry oneAddition1024 [m] q (ass0: [m][512][4]u64) (bss0: [m][512][4]u64) : [m][2*256][4]u64 = #[unsafe]
   let ass = ass0 :> [m][(2*256)][4]u64
   let bss = bss0 :> [m][(2*256)][4]u64
   let rss = imap2Intra ass bss (badd q) |> map unflatten
   in  rss

--   let apb   = imap2Intra ass bss badd |> map unflatten     -- a + b
--   let a2pb2 = imap2Intra apb apb badd |> map unflatten     -- 2 * (a + b) = 2a + 2b
--   let a2pb3 = imap2Intra a2pb2 bss badd |> map unflatten   -- 2 * (a + b) + b = 2a + 3b
--   let a4pb6 = imap2Intra a2pb3 a2pb3 badd |> map unflatten -- 2 * (2a + 3b) = 4a + 6b
--   let a6pb9 = imap2Intra a4pb6 a2pb3 badd |> map unflatten -- (4a + 6b) + (2a + 3b) = 6a + 9b
--   let rss   = imap2Intra a6pb9 bss badd    -- 6a + 10b
--            |> map unflatten
--   in  rss

