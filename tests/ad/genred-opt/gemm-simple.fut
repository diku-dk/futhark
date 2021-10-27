-- ==
-- entry: rev_J
-- compiled random input { 0.5f32 0.7f32 [1024][1024]f32 [1024][1024]f32 [1024][1024]f32 [1024][1024]f32} auto output

type real = f32
let real_sum = f32.sum

let dotprod xs ys = real_sum (map2 (*) xs ys)

let gemm [m][n][q] (alpha:real) (beta: real) (xss: [m][q]real, yss: [q][n]real, css: [m][n]real) =
  map2 (\xs cs -> map2 (\ys c -> c*alpha + beta*(dotprod xs ys)) (transpose yss) cs) xss css

entry rev_J [n][m][q] (alpha: real) (beta: real) (xss: [m][q]real) (yss: [q][n]real) (css: [m][n]real) (res_adj: [m][n]real) =
  vjp (gemm alpha beta) (xss, yss, css) res_adj


--- BIG COMMENT -------
--- The straigh compilation yields something like:
--- 
--- segmap(thread; #groups=num_groups_7382; groupsize=segmap_group_size_7381; virtualise)
---     (gtid_7195 < m_7102, gtid_7196 < n_7104, gtid_7197 < q_7103) (~phys_tid_7198) :
---     { acc(acc_cert_p_7123, [n_7104][q_7103], {f64}),
---       acc(acc_cert_p_7356, [m_7102][q_7103], {f64})
---     } {
---         let {binop_y_adj_7388 : f64} = binop_y_adj_r_r_7367[gtid_7195, gtid_7196]
---         let {x_7389 : f64} = xss_7107[gtid_7195, gtid_7197]
---         let {x_7390 : f64} = yss_coalesced_7414[gtid_7197, gtid_7196]
---         let {binop_x_adj_7394 : f64} = fmul64(binop_y_adj_7388, x_7390)
---         let {binop_y_adj_7395 : f64} = fmul64(binop_y_adj_7388, x_7389)
--- 
---         let {acc_7396 : acc(acc_cert_p_7356, [m_7102][q_7103], {f64})} =
---                 update_acc(acc_p_7357, {gtid_7195, gtid_7197}, {binop_x_adj_7394})
--- 
---         let {acc_7397 : acc(acc_cert_p_7123, [n_7104][q_7103], {f64})} =
---                 update_acc(acc_p_7124, {gtid_7196, gtid_7197}, {binop_y_adj_7395})
---         return {returns acc_7397, returns acc_7396}
---     }
--- 
---------------------------
