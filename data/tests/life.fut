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

fun int bint(bool b) = if b then 1 else 0
fun bool intb(int x) = if x == 0 then False else True

fun [][]bool to_bool_board([][]int board) =
  map(fn []bool ([]int r) => map(intb, r), board)

fun [][]int to_int_board([][]bool board) =
  map(fn []int ([]bool r) => map(bint, r), board)

fun int cell_neighbors(int i, int j, [n][m]bool board) =
  unsafe
  let above = (i - 1) % n in
  let below = (i + 1) % n in
  let right = (j + 1) % m in
  let left = (j - 1) % m in
  bint(board[left,above]) + bint(board[j,above]) + bint(board[right,above]) +
  bint(board[i,left]) + bint(board[i,right]) +
  bint(board[left,below]) + bint(board[j,below]) + bint(board[right,below])

fun [n][m]int all_neighbours([n][m]bool board) =
  map(fn []int (int i) =>
        map(fn int (int j) =>
              cell_neighbors(i,j,board),
            iota(m)),
        iota(n))

fun [n][m]bool iteration([n][m]bool board) =
  let lives = all_neighbours(board) in
  zipWith(fn []bool ([]int lives_r, []bool board_r) =>
            zipWith(fn bool (int neighbors, bool alive) =>
                      if neighbors < 2
                      then False
                      else if neighbors == 3 then True
                      else if alive && neighbors < 4 then True
                      else False,
                    lives_r, board_r),
            lives, board)

fun [][]int main([][]int int_board, int iterations) =
  -- We accept the board as integers for convenience, and then we
  -- convert to booleans here.
  let board = to_bool_board(int_board) in
  loop (board) = for i < iterations do
    iteration(board) in
  to_int_board(board)
