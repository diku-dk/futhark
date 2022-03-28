import "typeinst32"
import "tridiag"

def tridiag [xdim][ydim][zdim] (a: [xdim][ydim][zdim]DTYPE) (b: [xdim][ydim][zdim]DTYPE) (c: [xdim][ydim][zdim]DTYPE) (y: [xdim][ydim][zdim]DTYPE): *[xdim][ydim][zdim]DTYPE =
   map4 (\as bs cs ys ->
        map4 (\a b c y ->
            tridagPar (a,b,c,y)
        ) as bs cs ys
   ) a b c y

def limiter (cr: DTYPE) : DTYPE =
    real_max 0 (real_max (real_min 1 (2 * cr)) (real_min 2 cr))

def calcflux
    (velfac: DTYPE)
    (velS: DTYPE)
    (dt_tracer: DTYPE)
    (dx: DTYPE)
    (varS: DTYPE)
    (varSM1: DTYPE)
    (varSP1: DTYPE)
    (varSP2: DTYPE)
    (maskS: DTYPE)
    (maskSM1: DTYPE)
    (maskSP1: DTYPE)
    : DTYPE
  =
    let scaledVel =  velfac * velS
    let uCFL = real_abs (scaledVel * dt_tracer / dx)
    let rjp = (varSP2 - varSP1) * maskSP1
    let rj = (varSP1 - varS) * maskS
    let rjm = (varS - varSM1) * maskSM1
    let epsilon = 0.00000000000000000001
    let divisor = if (real_abs rj) < epsilon then epsilon else rj
    let cr = if velS>0 then rjm / divisor else rjp / divisor
    let cr = limiter(cr)
    in scaledVel * (varSP1 + varS) * 0.5 - (real_abs scaledVel) * ((1.0 - cr) + uCFL * cr) * rj * 0.5


def map3D 't [xdim] [ydim] [zdim] (arr: [xdim][ydim][zdim]t) (f : t -> i32 -> i32 -> i32 -> t) =
    map2(\ square i -> 
            map2(\ row j ->
                    map2 (\ el k -> f el i j k)
                         row (map i32.i64 (iota zdim))
                ) square (map i32.i64 (iota ydim))
        ) arr (map i32.i64 (iota xdim))

-- ==
-- entry: main
--
-- compiled random input { [200][200][100]f32 [200][200][100]f32 [200][200][100]f32 [200][200][100]f32 [200][200][100]f32 [200][200][100]f32 [200][200][100]f32 [200][200][100]f32 [200][200][100]f32 [200][200][100]f32 [200][200][100]f32 [200][200][100]f32 [200]f32 [200]f32 [200]f32 [200]f32 [100]f32 [100]f32 [200]f32 [200]f32 [200][200]i32 [200][200][100]f32 [200][200][100]f32 [200][200][100]f32 [200][200]f32}
--
-- compiled input @ data/tke32-small.in
-- output @ data/tke32-small.out
--

entry main [xdim][ydim][zdim]
        (tketau: * [xdim][ydim][zdim]DTYPE)
        (tketaup1:*[xdim][ydim][zdim]DTYPE)
        (tketaum1:*[xdim][ydim][zdim]DTYPE)
        (dtketau: *[xdim][ydim][zdim]DTYPE)
       (dtketaup1:*[xdim][ydim][zdim]DTYPE)
       (dtketaum1:*[xdim][ydim][zdim]DTYPE)
        (utau:     [xdim][ydim][zdim]DTYPE)
        -- (utaup1:   [xdim][ydim][zdim]DTYPE)
        -- (utaum1:   [xdim][ydim][zdim]DTYPE)
        (vtau:     [xdim][ydim][zdim]DTYPE)
        -- (vtaup1:   [xdim][ydim][zdim]DTYPE)
        -- (vtaum1:   [xdim][ydim][zdim]DTYPE)
        (wtau:     [xdim][ydim][zdim]DTYPE)
        -- (wtaup1:   [xdim][ydim][zdim]DTYPE)
        -- (wtaum1:   [xdim][ydim][zdim]DTYPE)
        (maskU:    [xdim][ydim][zdim]DTYPE)
        (maskV:    [xdim][ydim][zdim]DTYPE)
        (maskW:    [xdim][ydim][zdim]DTYPE)
        (dxt:      [xdim]            DTYPE)
        (dxu:      [xdim]            DTYPE)
        (dyt:            [ydim]      DTYPE)
        (dyu:            [ydim]      DTYPE)
        (dzt:                  [zdim]DTYPE)
        (dzw:                  [zdim]DTYPE)
        (cost:           [ydim]      DTYPE)
        (cosu:           [ydim]      DTYPE)
        (kbot:     [xdim][ydim]      i32  )
        (kappaM:   [xdim][ydim][zdim]DTYPE)
        (mxl:      [xdim][ydim][zdim]DTYPE)
        (forc:     [xdim][ydim][zdim]DTYPE)
        (forc_tke_surface:
                   [xdim][ydim]    DTYPE) =
        --  :
            -- (tke: *[3][h][w][d]DTYPE,
            -- dtke: *[3][h][w][d]DTYPE,
            -- tke_surf_corr: [h][w]DTYPE) =
    -- let tau = 0
    -- let taup1 = 1
    -- let taum1 = 2
    let dt_tracer = 1
    let dt_mom = 1
    let AB_eps = 0.1
    let alpha_tke = 1
    let c_eps = 0.7
    let K_h_tke = 2000
    let dt_tke = dt_mom

    -- Init delta
    let delta = tabulate_3d xdim ydim zdim (\x y z ->
                    if x >= 2 && x < xdim - 2 && y >= 2 && y < ydim - 2 && z < zdim-1
                    then dt_tke / dzt[z+1] * alpha_tke * 0.5
                        * (kappaM[x, y, z] + kappaM[x, y, z+1])
                    else 0
                )
    let a_tri = tabulate_3d xdim ydim zdim (\x y z ->
                        if x >= 2 && x < xdim - 2 && y >= 2 && y < ydim - 2
                        then
                            let ks_val = kbot[x,y]-1
                            let land_mask = ks_val >= 0
                            let edge_mask = land_mask && ((i32.i64 z) ==ks_val)
                            let water_mask = land_mask && ((i32.i64 z)>=ks_val)
                            in if edge_mask || (!water_mask)
                                then 0
                            else
                                if z > 0 && z < zdim-1
                                    then -delta[x,y,z-1] / dzw[z]
                                else if z == zdim-1
                                    then -delta[x, y, z-1] / (0.5 * dzw[z])
                                else 0
                        else 0
                )

    let b_tri = map3D tketau
        (\el_tketau x y z ->
            let sqrttke = real_sqrt (real_max 0 el_tketau) in
            if x >= 2i32 && x < (i32.i64 xdim) - 2i32 && y >= 2i32 && y < (i32.i64 ydim) - 2i32
            then
                        let ks_val = kbot[x,y]-1
                        let land_mask = ks_val >= 0
                        let edge_mask = land_mask && ( z == ks_val)
                        let water_mask = land_mask && (z >= ks_val)
                        in if !water_mask
                            then 1
                        else if edge_mask
                        then
                            1 + delta[x,y,z] / dzw[z]
                                + dt_tke * c_eps / mxl[x, y, z] * sqrttke
                        else
                            if z > 0i32 && z < (i32.i64 zdim)-1i32
                                then 1 + (delta[x, y, z] + delta[x, y, z-1]) / dzw[z]
                                    + dt_tke * c_eps
                                    * sqrttke / mxl[x, y, z]
                            else if z == (i32.i64 zdim) - 1
                                then 1 + delta[x, y, z-1] / (0.5 * dzw[z])
                                + dt_tke * c_eps / mxl[x,y,z] * sqrttke
                            else 0
            else 0
        )


--    let sqrttke = tabulate_3d xdim ydim zdim (\x y z ->
--                                        real_sqrt (real_max 0 tketau[x,y,z])
--                                    )
--
--    -- init b_tri
--    let b_tri = tabulate_3d xdim ydim zdim (\x y z ->
--                    if x >= 2 && x < xdim - 2 && y >= 2 && y < ydim - 2
--                    then
--                        let ks_val = kbot[x,y]-1
--                        let land_mask = ks_val >= 0
--                        let edge_mask = land_mask && ((i32.i64 z) ==ks_val)
--                        let water_mask = land_mask && ((i32.i64 z) >=ks_val)
--                        in if !water_mask
--                            then 1
--                        else if edge_mask
--                        then
--                            1 + delta[x,y,z] / dzw[z]
--                                + dt_tke * c_eps / mxl[x, y, z] * sqrttke[x, y, z]
--                        else
--                            if z > 0 && z < zdim-1
--                                then 1 + (delta[x, y, z] + delta[x, y, z-1]) / dzw[z]
--                                    + dt_tke * c_eps
--                                    * sqrttke[x, y, z] / mxl[x, y, z]
--                            else if z == zdim-1
--                                then 1 + delta[x, y, z-1] / (0.5 * dzw[z])
--                                + dt_tke * c_eps / mxl[x,y,z] * sqrttke[x,y,z]
--                            else 0
--                    else 0
--                )
    -- init b_tri_edge
    -- let b_tri_edge = tabulate_3d xdim ydim zdim (\x y z ->
    --                     if x >= 2 && x < xdim - 2 && y >= 2 && y < ydim - 2
    --                     then  1 + delta[x,y,z] / dzw[z]
    --                             + dt_tke * c_eps / mxl[x, y, z] * sqrttke[x, y, z]
    --                     else 0
    --                 )
    let c_tri = tabulate_3d xdim ydim zdim (\x y z ->
                    if x >= 2 && x < xdim - 2 && y >= 2 && y < ydim - 2 && z < zdim-1
                        then
                            let ks_val = kbot[x,y]-1
                            let land_mask = ks_val >= 0
                            let water_mask = land_mask && ((i32.i64 z) >=ks_val)
                            in if !water_mask
                                then
                                    0
                                else
                                    -delta[x,y,z] / dzw[z]
                        else 0
                )
    let d_tri = tabulate_3d xdim ydim zdim (\x y z ->
                    if x >= 2 && x < xdim - 2 && y >= 2 && y < ydim - 2
                        then
                            let ks_val = kbot[x,y]-1
                            let land_mask = ks_val >= 0
                            let water_mask = land_mask && ((i32.i64 z) >=ks_val)
                            in if !water_mask
                            then
                                0
                            else
                                let tmp = tketau[x,y,z] + dt_tke * forc[x,y,z]
                                in if z == zdim-1
                                    then tmp + dt_tke * forc_tke_surface[x,y] / (0.5 * dzw[z])
                                    else tmp
                        else 0
                )
    let sol = tridiag a_tri b_tri c_tri d_tri



    let tketaup1 = tabulate_3d xdim ydim zdim (\x y z ->
                let ks_val = kbot[x,y]-1
                let water_mask = (ks_val >= 0) && ((i32.i64 z) >=ks_val) in
                    if x >= 2 && x < xdim - 2 && y >= 2 && y < ydim - 2 && water_mask
                        then sol[x,y,z]
                        else tketaup1[x,y,z]
                )
    let tke_surf_corr = tabulate_2d xdim ydim (\x y ->
                    if x >= 2 && x < xdim - 2 && y >= 2 && y < ydim - 2
                    then
                        let tke_val = tketaup1[x, y, zdim-1] in
                            if tke_val < 0
                            then -tke_val * 0.5 * dzw[zdim-1] / dt_tke
                            else 0
                    else 0
                    )
    -- clip negative vals on last z val
    let tketaup1 = tabulate_3d xdim ydim zdim (\x y z ->
                         if x >= 2 && x < xdim - 2 && y >= 2 && y < ydim - 2 && z == zdim-1
                         then
                            let tke_val = tketaup1[x,y,z] in
                            if tke_val < 0
                                then 0
                                else tke_val
                        else tketaup1[x,y,z]
                    )
    -- lateral diff east
    let flux_east = tabulate_3d xdim ydim zdim (\x y z ->
                        if x < xdim-1
                        then
                            K_h_tke * (tketau[x+1, y, z] - tketau[x, y, z])
                                / (cost[y] * dxu[x]) * maskU[x, y, z]
                        else 0

    )
    -- lateral diff north
    let flux_north = tabulate_3d xdim ydim zdim (\x y z ->
                        if y < ydim-1
                        then
                            K_h_tke * (tketau[x, y+1, z] - tketau[x, y, z])
                                / dyu[y] * maskV[x, y, z] * cosu[y]
                        else 0
                    )
    -- add lateral diffusion
    let tketaup1 = tabulate_3d xdim ydim zdim (\x y z ->
                        let previous = tketaup1[x,y,z] in
                        if x >= 2 && x < xdim - 2 && y >= 2 && y < ydim - 2 && z == zdim-1
                        then previous + dt_tke * maskW[x, y, z] *
                                ((flux_east[x,y,z] - flux_east[x-1, y, z])
                                / (cost[y] * dxt[x])
                                + (flux_north[x,y,z] - flux_north[x, y-1, z])
                                / (cost[y] * dyt[y]))
                        else previous
                    )
    -- tendency due to advection
    let flux_east = tabulate_3d xdim ydim zdim (\x y z ->
                        if x >= 1 && x < xdim - 2 && y >= 2 && y < ydim - 2
                        then
                            let dx = cost[y] * dxt[x]
                            let velS = utau[x,y,z]

                            let maskWm1 = maskW[x-1,y,z]
                            let maskWs = maskW[x,y,z]
                            let maskWp1 = maskW[x+1,y,z]
                            let maskwp2 = maskW[x+2,y,z]
                            let varSM1 = tketau[x-1,y,z]
                            let varS = tketau[x,y,z]
                            let varSP1 = tketau[x+1,y,z]
                            let varSP2 = tketau[x+2,y,z]
                            let maskUtr = if x < xdim-1 then maskWs * maskWp1 else 0
                            let maskUtrP1 = if x < xdim-1 then maskWp1 * maskwp2 else 0
                            let maskUtrM1 = if x < xdim-1 then maskWm1 * maskWs else 0
                            in calcflux 1 velS dt_tracer dx varS varSM1 varSP1 varSP2 maskUtr maskUtrM1 maskUtrP1
                        else 0
                    )
    let flux_north = tabulate_3d xdim ydim zdim (\x y z ->
                        if y >= 1 && y < ydim - 2 && x >= 2 && x < xdim-2
                        then
                            let dx = cost[y] * dyt[y]
                            let velS = vtau[x,y,z]
                            let maskWm1 = maskW[x,y-1,z]
                            let maskWs = maskW[x,y,z]
                            let maskWp1 = maskW[x,y+1,z]
                            let maskwp2 = maskW[x,y+2,z]
                            let varSM1 = tketau[x,y-1,z]
                            let varS = tketau[x,y,z]
                            let varSP1 = tketau[x,y+1,z]
                            let varSP2 = tketau[x,y+2,z]
                            let maskVtr = if y < ydim-1 then maskWs * maskWp1 else 0
                            let maskVtrP1 = if y < ydim-1 then maskWp1 * maskwp2 else 0
                            let maskVtrM1 = if y < ydim-1 then maskWm1 * maskWs else 0
                            in calcflux cosu[y] velS dt_tracer dx varS varSM1 varSP1 varSP2 maskVtr maskVtrM1 maskVtrP1
                        else 0
                    )
    let flux_top = tabulate_3d xdim ydim zdim (\x y z ->
                        if z < zdim-1 && x >= 2 && x < xdim-2 && y >= 2 && y < ydim-2
                        then
                            let velS = wtau[x,y,z]
                            let varSM1 = if z != 0 then tketau[x,y,z-1] else 0
                            let varS = tketau[x,y,z]
                            let varSP2 = if z < zdim-2 then tketau[x,y,z+2] else 0
                            let varSP1 = tketau[x,y,z+1]
                            let maskWm1 = if z != 0 then maskW[x,y,z-1] else 0
                            let maskWs = maskW[x,y,z]
                            let maskWp1 = maskW[x,y,z+1]
                            let maskwp2 = if z < zdim-2 then maskW[x,y,z+2] else 0
                            let maskWtr = maskWs * maskWp1
                            let maskWtrP1 = maskWp1 * maskwp2
                            let maskWtrM1 = maskWm1 * maskWs
                            let dx = dzw[z]
                            in calcflux 1 velS dt_tracer dx varS varSM1 varSP1 varSP2 maskWtr maskWtrM1 maskWtrP1
                        else 0
                    )
    let dtketau = tabulate_3d xdim ydim zdim (\x y z ->
                    let tmp = 
                        if x >= 2 && x < xdim - 2 && y >= 2 && y < ydim - 2
                        then
                            maskW[x,y,z] * (-(flux_east[x,y,z] - flux_east[x-1, y, z])
                                / (cost[y] * dxt[x])
                                - (flux_north[x,y,z] - flux_north[x, y-1, z])
                                / (cost[y] * dyt[y]))
                        else dtketau[x,y,z]
                    in 
                        let z0_update = if z == 0 then tmp - flux_top[x, y, 0] / dzw[0] else tmp
                        let z_middle_update = if z >= 1 && z < zdim-1 
                                                then z0_update - (flux_top[x, y, z] - flux_top[x, y, z-1]) / dzw[z]
                                                else z0_update
                        in if z == zdim-1 then z_middle_update - (flux_top[x, y, z] - flux_top[x, y, z-1]) /
                                                        (0.5 * dzw[z])
                                          else z_middle_update
                )


    let tketaup1 = tabulate_3d xdim ydim zdim (\x y z ->
                        tketaup1[x,y,z] + dt_tracer * ((1.5 + AB_eps) * dtketau[x, y, z] - (0.5 + AB_eps) * dtketaum1[x, y, z])
                    )    
    in (tketau, tketaup1, tketaum1, dtketau, dtketaup1, dtketaum1, tke_surf_corr)
