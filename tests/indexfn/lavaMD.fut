-- Code and comments based on
-- https://github.com/kkushagra/rodinia/blob/master/openmp/lavaMD/main.c
--
-- ==
-- input @ data/3_boxes.in
-- output @ data/3_boxes.out
-- compiled input @ data/10_boxes.in.gz
-- output @ data/10_boxes.out.gz

def dot ((ax, ay, az), (bx, by, bz)) : f32 =
  ax * bx + ay * by + az * bz

def f a2 rai_v rai_x rai_y rai_z rbj_v rbj_x rbj_y rbj_z qbj
    : {(f32, f32, f32, f32)| \_ -> true}=
  let r2 = rai_v + rbj_v - dot ((rai_x, rai_y, rai_z), (rbj_x, rbj_y, rbj_z))
  let u2 = a2 * r2
  let vij = f32.exp (-u2)
  let fs = 2 * vij
  let d_x = rai_x - rbj_x
  let d_y = rai_y - rbj_y
  let d_z = rai_z - rbj_z
  let fxij = fs * d_x
  let fyij = fs * d_y
  let fzij = fs * d_z
  in (qbj * vij, qbj * fxij, qbj * fyij, qbj * fzij)

def neighbor [par_per_box]
             (a2: f32)
             (rai_v: f32)
             (rai_x: f32)
             (rai_y: f32)
             (rai_z: f32)
             (rB_0: [par_per_box]f32)
             (rB_1: [par_per_box]f32)
             (rB_2: [par_per_box]f32)
             (rB_3: [par_per_box]f32)
             (qB: [par_per_box]f32) : {(f32, f32, f32, f32) | \_ -> true}=
  let rB = zip4 rB_0 rB_1 rB_2 rB_3
  let pres =
    map2 (\(rbj_v, rbj_x, rbj_y, rbj_z) qbj ->
            f a2 rai_v rai_x rai_y rai_z rbj_v rbj_x rbj_y rbj_z qbj)
         rB
         qB
  let (y0, y1, y2, y3) = unzip4 (scan (\(a1, a2, a3, a4) (b1, b2, b3, b4) -> (a1 + b1, a2 + b2, a3 + b3, a4 + b4))
                   (0f32, 0f32, 0f32, 0f32)
                   pres)
  in if par_per_box > 0
     then (y0[par_per_box - 1], y1[par_per_box - 1], y2[par_per_box - 1], y3[par_per_box - 1])
     else (0f32, 0, 0, 0)

-----------------------------------------
-- Main Computational Kernel of lavaMD --
-----------------------------------------
def main [number_boxes] [par_per_box] [num_neighbors]
         (alpha: f32)
         (_box_coefs_0: [number_boxes]i64)
         (_box_coefs_1: [number_boxes]i64)
         (_box_coefs_2: [number_boxes]i64)
         (box_coefs_3: {[number_boxes]i64 | \x -> Range x (0,number_boxes)})
         (_box_nnghs_0: [num_neighbors][number_boxes]i64)
         (_box_nnghs_1: [num_neighbors][number_boxes]i64)
         (_box_nnghs_2: [num_neighbors][number_boxes]i64)
         (box_nnghs_3: {[num_neighbors][number_boxes]i64 | \x -> Range x (0,number_boxes)})
         (box_num_nghbs: {[number_boxes]i64 | \x -> Range x (0,num_neighbors)})
         (rv_0: [number_boxes][par_per_box]f32)
         (rv_1: [number_boxes][par_per_box]f32)
         (rv_2: [number_boxes][par_per_box]f32)
         (rv_3: [number_boxes][par_per_box]f32)
         (qv: [number_boxes][par_per_box]f32) : {( [number_boxes][par_per_box]f32
                                                , [number_boxes][par_per_box]f32
                                                , [number_boxes][par_per_box]f32
                                                , [number_boxes][par_per_box]f32
                                                ) | \_ -> true} =
  let a2 = 2 * alpha * alpha
  in unzip4 (
    map2 (\box_num_nghbs' (l: i64) ->
      unzip4 (map4 (\rA_el_0 rA_el_1 rA_el_2 rA_el_3 ->
             let acc = (0, 0, 0, 0)
             in loop (acc) for k < box_num_nghbs' + 1 do
                  let pointer =
                    if (k > 0)
                    then let num = box_nnghs_3[k - 1, l] in num
                    else l
                  let first_j = box_coefs_3[pointer]
                  let rB_0 = rv_0[first_j, :]
                  let rB_1 = rv_1[first_j, :]
                  let rB_2 = rv_2[first_j, :]
                  let rB_3 = rv_3[first_j, :]
                  let qB = qv[first_j, :]
                  let (r1, r2, r3, r4) = neighbor a2 rA_el_0 rA_el_1 rA_el_2 rA_el_3 rB_0 rB_1 rB_2 rB_3 qB
                  let (a1, a2, a3, a4) = acc
                  in (a1 + r1, a2 + r2, a3 + r3, a4 + r4))
          rv_0[l, :] rv_1[l, :] rv_2[l, :] rv_3[l, :]))
         box_num_nghbs
         (iota (number_boxes)))
