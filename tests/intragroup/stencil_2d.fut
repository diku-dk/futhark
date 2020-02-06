-- Intragroup Game of Life!
-- ==
-- random no_python compiled input { 100 [100][16][16]bool } auto output
-- random no_python compiled input { 1000 [100][16][16]bool } auto output
-- random no_python compiled input { 3000 [100][16][16]bool } auto output

let bint: bool -> i32 = i32.bool

let all_neighbours [n][m] (world: [n][m]bool): [n][m]i32 =
    let ns  = map (rotate (-1)) world
    let ss  = map (rotate   1)  world
    let ws  = rotate      (-1)  world
    let es  = rotate        1   world
    let nws = map (rotate (-1)) ws
    let nes = map (rotate (-1)) es
    let sws = map (rotate   1)  ws
    let ses = map (rotate   1)  es
    in map3 (\(nws_r, ns_r, nes_r) (ws_r, world_r, es_r) (sws_r, ss_r, ses_r) ->
             map3 (\(nw,n,ne) (w,_,e) (sw,s,se) ->
                   bint nw + bint n + bint ne +
                   bint w + bint e +
                   bint sw + bint s + bint se)
             (zip3 nws_r ns_r nes_r) (zip3 ws_r world_r es_r) (zip3 sws_r ss_r ses_r))
            (zip3 nws ns nes) (zip3 ws world es) (zip3 sws ss ses)

let iteration [n][m] (board: [n][m]bool): [n][m]bool =
  let lives = all_neighbours(board) in
  map2 (\(lives_r: []i32) (board_r: []bool)  ->
            map2 (\(neighbors: i32) (alive: bool): bool  ->
                      if neighbors < 2
                      then false
                      else if neighbors == 3 then true
                      else if alive && neighbors < 4 then true
                      else false)
                    lives_r board_r)
           lives board

let life (iterations: i32) (board: [][]bool) =
  loop board for _i < iterations do iteration board

let main (iterations: i32) (board: [][][]bool) =
  map (life iterations) board
