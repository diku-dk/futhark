-- Simple game of life implementation with a donut world.  Tested with
-- a glider running for four iterations.
--
-- http://rosettacode.org/wiki/Conway's_Game_of_Life
--
-- ==
-- input {
--   [[0, 0, 0, 0, 0],
--    [0, 0, 1, 0, 0],
--    [0, 0, 0, 1, 0],
--    [0, 1, 1, 1, 0],
--    [0, 0, 0, 0, 0]]
--   4
--   }
-- output {
--   [[0, 0, 0, 0, 0],
--    [0, 0, 0, 0, 0],
--    [0, 0, 0, 1, 0],
--    [0, 0, 0, 0, 1],
--    [0, 0, 1, 1, 1]]
--   }
-- input {
--   [[0, 0, 0, 0, 0],
--    [0, 0, 1, 0, 0],
--    [0, 0, 0, 1, 0],
--    [0, 1, 1, 1, 0],
--    [0, 0, 0, 0, 0]]
--   8
--   }
-- output {
--   [[1, 0, 0, 1, 1],
--    [0, 0, 0, 0, 0],
--    [0, 0, 0, 0, 0],
--    [0, 0, 0, 0, 1],
--    [1, 0, 0, 0, 0]]
--   }

let bint(b: bool): i32 = if b then 1 else 0
let intb(x: i32): bool = if x == 0 then false else true

let to_bool_board(board: [][]i32): [][]bool =
  map (\(r: []i32): []bool  -> map intb r) board

let to_int_board(board: [][]bool): [][]i32 =
  map (\(r: []bool): []i32  -> map bint r) board

let all_neighbours [n][m] (world: [n][m]bool): [n][m]i32 =
    let world = map (map bint) world
    let ns  = map (rotate (-1)) world
    let ss  = map (rotate   1)  world
    let ws  = rotate      (-1)  world
    let es  = rotate        1   world
    let nws = map (rotate (-1)) ws
    let nes = map (rotate (-1)) es
    let sws = map (rotate   1)  ws
    let ses = map (rotate   1)  es
    in map3 (\(nws_r, ns_r, nes_r) (ws_r, world_r, es_r) (sws_r, ss_r, ses_r) ->
             map3 (\(nw,n,ne) (w,_,e) (sw,s,se) -> nw + n + ne + w + e + sw + s + se)
             (zip3 nws_r ns_r nes_r) (zip3 ws_r world_r es_r) (zip3 sws_r ss_r ses_r))
            (zip3 nws ns nes) (zip3 ws world es) (zip3 sws ss ses)

let iteration [n][m] (board: [n][m]bool): [n][m]bool =
  let lives = all_neighbours(board) in
  map2 (\(lives_r: []i32) (board_r: []bool): []bool  ->
            map2 (\(neighbors: i32) (alive: bool): bool  ->
                      if neighbors < 2
                      then false
                      else if neighbors == 3 then true
                      else if alive && neighbors < 4 then true
                      else false)
                    lives_r board_r)
           lives board

let main (int_board: [][]i32) (iterations: i32): [][]i32 =
  -- We accept the board as integers for convenience, and then we
  -- convert to booleans here.
  let board = to_bool_board int_board
  in to_int_board (loop board for _i < iterations do iteration board)
