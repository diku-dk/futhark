-- Simple game of life implementation with a donut world.  Tested with
-- a glider running for four iterations.
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

fun bint(b: bool): int = if b then 1 else 0
fun intb(x: int): bool = if x == 0 then False else True

fun to_bool_board(board: [][]int): [][]bool =
  map (fn (r: []int): []bool  => map intb r) board

fun to_int_board(board: [][]bool): [][]int =
  map (fn (r: []bool): []int  => map bint r) board

fun cell_neighbors(i: int, j: int, board: [n][m]bool): int =
  unsafe
  let above = (i - 1) % n in
  let below = (i + 1) % n in
  let right = (j + 1) % m in
  let left = (j - 1) % m in
  bint(board[above,left]) + bint(board[above,j]) + bint(board[above,right]) +
  bint(board[i,left]) + bint(board[i,right]) +
  bint(board[below,left]) + bint(board[below,j]) + bint(board[below,right])

fun all_neighbours(board: [n][m]bool): [n][m]int =
  map (fn (i: int): []int  =>
        map (fn (j: int): int  =>
              cell_neighbors(i,j,board)) (
            iota(m))) (
        iota(n))

fun iteration(board: [n][m]bool): [n][m]bool =
  let lives = all_neighbours(board) in
  zipWith (fn (lives_r: []int, board_r: []bool): []bool  =>
            zipWith (fn (neighbors: int, alive: bool): bool  =>
                      if neighbors < 2
                      then False
                      else if neighbors == 3 then True
                      else if alive && neighbors < 4 then True
                      else False) (
                    lives_r) (board_r)) lives board

fun main(int_board: [][]int, iterations: int): [][]int =
  -- We accept the board as integers for convenience, and then we
  -- convert to booleans here.
  let board = to_bool_board(int_board) in
  loop (board) = for i < iterations do
    iteration(board) in
  to_int_board(board)
