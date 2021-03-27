-- Compute total hydrogen bonding energy for configuration
--               bb      bb/sc
--  position [[N,H] x [O, C]]
--  :param sites: cartesian coordinates of all atoms in peptide
--  :param tuple hydrogen_groups: (nh positions, oc positions)
--  :param bonding_relation: n_pos,h_pos ~ o_pos, c_pos
--  :return: hydrogen bonding energy for polypeptide
--

let dotprod [q] (v: [q]f32) (w: [q]f32) =
    map2 (*) v w |> reduce (+) 0f32

let norm (v: [3]f32) =
    f32.sqrt (v[0]*v[0] + v[1]*v[1] + v[2]*v[2])

let cos_angle (v1: [3]f32) (v2: [3]f32) (v3: [3]f32) =
    let v = map2 (-) v1 v2
    let w = map2 (-) v2 v3
    in  (dotprod v w) / ( (norm v) * (norm w) )

let potential_energy (dist: f32) =
    let sigma_hb = 2.0
    in  5 * ((sigma_hb / dist) ** 12) - (6 * ( (sigma_hb / dist) ** 10) )

let kinetic_energy (cos_angle1: f32) (cos_angle2: f32) =
    let prod = cos_angle1 * cos_angle2
    in  if prod > 0
        then f32.sqrt prod
        else 0f32

let bond_energy (n_pos: [3]f32, h_pos: [3]f32) (o_pos: [3]f32, c_pos: [3]f32) =
    let cos_noc_angle = cos_angle n_pos h_pos o_pos
    let cos_hoc_angle = cos_angle h_pos o_pos c_pos
    let dist = norm <| map2 (-) h_pos o_pos
    in  (potential_energy dist) * (kinetic_energy cos_noc_angle cos_hoc_angle)

let hydrogen_bonding [n][m][q] (sites: [n][3]f32)
                               (bb_amino_pos: [m](i64,i64))
                               (bb_acid_pos:  [q](i64,i64))
                               (weights: [q]f32) =
    -- bb_amino_coords = sites[bb_amino_pos.ravel()].reshape((*bb_amino_pos.shape, -1))
    let bb_amino_coords = map (\(n',h') -> (sites[n'], sites[h']) ) bb_amino_pos
    let bb_acid_coords  = map (\(o',c') -> (sites[o'], sites[c']) ) bb_acid_pos
    --energies = vmap(lambda amino: weights @ vmap(lambda acid: bond_energy(amino, acid))(bb_acid_coords))(
    --  bb_amino_coords)  # nested loop
    in  map (\ amino ->
              let hbond_energies = map (\ acid -> bond_energy amino acid) bb_acid_coords
              in  dotprod weights hbond_energies
            ) bb_amino_coords
      |> reduce (+) 0.0f32

let main [n][m][q] (sites: [n][3]f32)
                   (bb_amino_pos_1: [m]i64)
                   (bb_amino_pos_2: [m]i64)
                   (bb_acid_pos_1:  [q]i64)
                   (bb_acid_pos_2:  [q]i64)
                   (weights: [q]f32) =
  hydrogen_bonding sites
                   (zip bb_amino_pos_1 bb_amino_pos_2)
                   (zip bb_acid_pos_1  bb_acid_pos_2 )
                   weights
