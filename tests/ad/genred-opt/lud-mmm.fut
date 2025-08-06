-- ==
--
-- compiled random input {[128][32][32]f32 [128][32][32]f32 [128][128][32][32]f32 [128][128][32][32]f32} auto output

-- Alternatively try this with default_tile_size=8 and default_reg_tile_size=2
-- compiled random input {[256][16][16]f32 [256][16][16]f32 [256][256][16][16]f32} auto output

def ludMult [m] [b] (top_per: [m][b][b]f32, lft_per: [m][b][b]f32, mat_slice: [m][m][b][b]f32) : *[m][m][b][b]f32 =
  -- let top_slice = map transpose top_per in
  map (\(mat_arr: [m][b][b]f32, lft: [b][b]f32) : [m][b][b]f32 ->
         map (\(mat_blk: [b][b]f32, top: [b][b]f32) : [b][b]f32 ->
                map (\(mat_row: [b]f32, lft_row: [b]f32) : [b]f32 ->
                       map2 (\mat_el top_row ->
                               let prods = map2 (*) lft_row top_row
                               let sum = f32.sum prods
                               in mat_el - sum)
                            mat_row
                            (transpose top))
                    (zip (mat_blk) lft))
             (zip (mat_arr) (top_per)))
      (zip (mat_slice) (lft_per))

def main [m] [b]
         (top_per: [m][b][b]f32)
         (lft_per: [m][b][b]f32)
         (mat_slice: [m][m][b][b]f32)
         (res_adj: [m][m][b][b]f32) =
  vjp ludMult (top_per, lft_per, mat_slice) res_adj

--- BIG COMMENT -------
--- The straigh compilation yields something like:
---
--- segmap(thread; #groups=num_tblocks_10633; tblocksize=segmap_tblock_size_10632; virtualise)
---     (gtid_9445 < m_9078, gtid_9446 < m_9078, gtid_9447 < b_9079, gtid_9448 < b_9079, gtid_9449 < b_9079) (~phys_tid_9450) :
---     { acc(acc_cert_p_10557, [m_9078][m_9078][b_9079][b_9079], {f32}),
---       acc(acc_cert_p_10590, [m_9078][m_9078][b_9079][b_9079], {f32})
---     } {
---
---         let {r_adj_el : f32} = r_adj[gtid_9445, gtid_9446, gtid_9447, gtid_9448]
---         let {lft_el : f32} = lft_per_9081[gtid_9445, gtid_9447, gtid_9449]
---         let {top_el : f32} = top_per_coalesced_10752[gtid_9446, gtid_9449, gtid_9448]
---
---         let {acc_10651 : acc(acc_cert_p_10590, [m_9078][m_9078][b_9079][b_9079], {f32})} =
---             update_acc(acc_p_10591, {gtid_9445, gtid_9446, gtid_9447, gtid_9449}, {r_adj_el * top_el})
---
---         let {acc_10652 : acc(acc_cert_p_10557, [m_9078][m_9078][b_9079][b_9079], {f32})} =
---             update_acc(acc_p_10558, {gtid_9445, gtid_9446, gtid_9448, gtid_9449}, {r_adj_el * lft_el})
---
---         return {returns acc_10652, returns acc_10651}
---     }
---
---     in {withacc_inter_10636, withacc_inter_10635})
---
---
--- segmap(thread; #groups=num_tblocks_10675; tblocksize=segmap_tblock_size_10674; virtualise)
---     (gtid_9334 < m_9078, gtid_9335 < m_9078, gtid_9336 < b_9079, gtid_9337 < b_9079) (~phys_tid_9338) :
---     { acc(acc_cert_p_10532, [m_9078][b_9079][b_9079], {f32}) }
---     {
---         let {r_adj_el : f32} = r_adj[gtid_9334, gtid_9335, gtid_9336, gtid_9337]
---         let {acc_10683 : acc(acc_cert_p_10532, [m_9078][b_9079][b_9079], {f32})} =
---           update_acc(acc_p_10533, {gtid_9334, gtid_9336, gtid_9337}, {r_adj_el})
---         return {returns acc_10683}
---     }
--------------------------------------------------
