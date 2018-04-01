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

let cell_neighbors [n] [m] (i: i32, j: i32, board: [n][m]bool): i32 =
  unsafe
  let above = (i - 1) % n
  let below = (i + 1) % n
  let right = (j + 1) % m
  let left = (j - 1) % m in
  bint board[above,left] + bint board[above,j]  + bint board[above,right] +
  bint board[i,left] + bint board[i,right] +
  bint board[below,left] + bint board[below,j] + bint board[below,right]

let all_neighbours [n] [m] (board: [n][m]bool): [n][m]i32 =
  map (\(i: i32): []i32  ->
        map (\(j: i32): i32  -> cell_neighbors(i,j,board)) (iota m))
      (iota n)

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

let main(int_board: [][]i32, iterations: i32): [][]i32 =
  -- We accept the board as integers for convenience, and then we
  -- convert to booleans here.
  let board = to_bool_board int_board
  in to_int_board (loop board for _i < iterations do iteration board)
