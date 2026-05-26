let imap  as f = map f as
let imap2 as bs f = map2 f as bs
let imap2Intra as bs f = #[incremental_flattening(only_intra)] map2 f as bs


-----------------------------------------------------------------------------
--- Implementation took inspiration from:
--- [1] Amar Topalovic, Walter Restelli-Nielsen, Kristian Olesen:
---     ``Multiple-precision Integer Arithmetic'', DPP'22 final project,
---     https://futhark-lang.org/student-projects/dpp21-mpint.pdf
-----------------------------------------------------------------------------

type cT         = u32      --u8
let  cTfromBool = u32.bool --u8.bool
let  two_cT     = 2u32     --2u8

------------------------------------------------------------------------
---- prefix sum (scan) operator to propagate the carry
-- let add_op (ov1 : bool, mx1: bool) (ov2 : bool, mx2: bool) : (bool, bool) =
--   ( (ov1 && mx2) || ov2,    mx1 && mx2 )
------------------------------------------------------------------------

---- prefix sum (scan) operator to propagate the curry:
---- format: last digit set      => overfolow
----         ante-last digit set => one unit away from overflowing   
-- let badd_op (c1 : u8) (c2: u8) : u8 =
let carryOp (c1: cT) (c2: cT) =
  (c1 & c2 & 2) | (( (c1 & (c2 >> 1)) | c2) & 1)
  
let carrySegOp (c1: cT) (c2: cT) =
    if (c2 & 4) != 0 then c2
    else let res = ( (c1 & (c2 >> 1)) | c2 ) & 1
         let res = res | (c1 & c2  & 2)
         in  ( res | ( (c1 | c2) & 4 ) )

let carryOpNE: cT = two_cT

let addPairwiseNice (is_seg_start: bool) (a: u64) (b: u64) : (u64, cT)=
  let r = a + b
  let c = cTfromBool (r < a)
  let c = c | ( (cTfromBool (r == u64.highest)) << 1 )
  let c = c | ( (cTfromBool is_seg_start ) << 2 )
  in  (r, c)

let badd0 [ipb][n] (areg : [ipb*n][4]u64) (breg : [ipb*n][4]u64) : [ipb*n][4]u64 =
  let nn = i32.i64 n
  let g = ipb * n
  
  let seqscan1 (tid: i32) (i: i32) (carry: cT) = #[unsafe]
    let a = areg[tid, i]
    let b = breg[tid, i]
    let is_seg_start = (tid * 4 + i) % (4 * nn) == 0
    let (r0, c0) = addPairwiseNice is_seg_start a b
    let r0 = r0 + u64.bool ( ( (c0 & 4) == 0 ) && ( (carry & 1) == 1 ) ) 
    in  (r0, carrySegOp carry c0)

  let (apbs, cs, carries_part) = opaque <| unzip3 <|
    #[toregmem(1)] map
       (\ tid64 ->
         let tid = i32.i64 tid64
         let carry = carryOpNE
         let cs = #[scratch] replicate 4 carryOpNE
         let rs = #[scratch] replicate 4 0u64 in
         loop (rs, cs, carry) for i < 4i32 do
           let (ri, carry) = seqscan1 tid i carry
           let rs[i] = ri
           let cs[i] = carry
           in  (rs, cs, carry)
       ) <| iota g
  
  let carries = scan carrySegOp carryOpNE carries_part |> opaque

  let rs = #[toregmem(1)] map
      (\ tid64 ->
        let tid = i32.i64 tid64
        let carry = if tid == 0 then carryOpNE else #[unsafe] carries[tid-1]
        let rs = #[scratch] replicate 4 0u64 in
        (loop (rs, carry) for i < 4i32 do
          let ci = cs[tid][i]
          let term = u64.bool ( (ci & 4) == 0 && (carry & 1) == 1 )
          let rs[i] = apbs[tid, i] + term
          in  (rs, carrySegOp carry ci)).0
      ) <| iota g

  in  rs

let badd [ipb][n] (as : [(ipb*n)][4]u64) (bs : [(ipb*n)][4]u64) : [(ipb*n)*4]u64 =
  let ash = #[glb2reg_only(1)] manifest as
  let bsh = #[glb2reg_only(1)] manifest bs

  --let ash = opaque (#[toregmem(1)] as)
  --let bsh = opaque (#[toregmem(1)] bs) 
  in  flatten (badd0 ash bsh)

---- ==
---- entry: oneAddition1024
---- compiled random input { [32768][512][4]u64  [32768][512][4]u64 } auto output
--entry oneAddition1024 [m] (ass0: [m][512][4]u64) (bss0: [m][512][4]u64) : [m][2*256][4]u64 = #[unsafe]
--   let ass = ass0 :> [m][(2*256)][4]u64
--   let bss = bss0 :> [m][(2*256)][4]u64
--   let rss = imap2Intra ass bss badd |> map unflatten
--   in  rss

-- ==
-- entry: oneAddition1024
-- compiled random input { [32768][512][4]u64  [32768][512][4]u64 } auto output
entry oneAddition1024 [m] (ass0: [m][512][4]u64) (bss0: [m][512][4]u64) : [m][2*256][4]u64 = #[unsafe]
   let ass = ass0 :> [m][(2*256)][4]u64
   let bss = bss0 :> [m][(2*256)][4]u64
--   let rss = imap2Intra ass bss badd |> map unflatten
--   in  rss
   let apb   = imap2Intra ass bss badd |> map unflatten     -- a + b
   let a2pb2 = imap2Intra apb apb badd |> map unflatten     -- 2 * (a + b) = 2a + 2b
   let a2pb3 = imap2Intra a2pb2 bss badd |> map unflatten   -- 2 * (a + b) + b = 2a + 3b
   let a4pb6 = imap2Intra a2pb3 a2pb3 badd |> map unflatten -- 2 * (2a + 3b) = 4a + 6b
   let a6pb9 = imap2Intra a4pb6 a2pb3 badd |> map unflatten -- (4a + 6b) + (2a + 3b) = 6a + 9b
   let rss   = imap2Intra a6pb9 bss badd    -- 6a + 10b
            |> map unflatten
   in  rss



---- ==
---- entry: twoAdditions512
---- compiled random input { [65536][256][4]u64  [65536][256][4]u64 }
-- entry twoAdditions512 [m] (ass0: [m][256][4]u64) (bss0: [m][256][4]u64) : [m][2*128][4]u64 = #[unsafe]
--   let ass = ass0 :> [m][(2*128)][4]u64
--   let bss = bss0 :> [m][(2*128)][4]u64
--   let rss = imap2Intra ass bss badd |> map unflatten
   -- let rss = imap2Intra ass rss badd |> map unflatten
--   in  rss
