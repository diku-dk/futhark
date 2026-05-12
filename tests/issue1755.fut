-- ==
-- input{} auto output

type turn = bool

module game = {
  type position = {score: i32, seed: i64}
  type move = #move i32 | #nomove
  def pos_size = 2i64
  def move_size = 2i64

  def legal_moves (position: position) turn : []move =
    let minmax = if turn then 1 else -1
    let move_num = 2 + position.seed % 2
    let nums = map (\r -> (r + 1, i32.i64 (r % 10))) (map (^ position.seed) (iota move_num))
    in map (\(_, x) -> #move (minmax * x)) nums

  def make_move (position: position) (move: move) : position =
    match move
    case #nomove -> position
    case #move move ->
      {score = position.score + move, seed = position.seed + 100 + i64.i32 move}

  def neutral_position : position =
    {score = 0, seed = 0}

  def add_position (a: position) (b: position) : position =
    {score = a.score + b.score, seed = a.seed + b.seed}
}

def bf_ab (position: game.position) (turn: turn) (depth: i64) =
  let (poss, _t, _shapes, _SS) =
    loop (poss, t, shapes, SS) = ([position], turn, [], [])
    for _ in 0..<depth do
      let S = map (\p -> length (game.legal_moves p t)) poss
      let Sscan = scan (+) 0 S
      let total_moves = last Sscan
      let B = rotate (-1) Sscan with [0] = 0
      let scatter_poss pos offset =
        #[sequential]
        let moves = game.legal_moves pos t
        let results = map (game.make_move pos) moves
        let is = map (+ offset) <| indices results
        let empty = replicate total_moves game.neutral_position
        in scatter empty is results
      let chunks = map2 scatter_poss poss B
      let ne = replicate total_moves game.neutral_position
      let new_poss = reduce (map2 game.add_position) ne chunks
      in (new_poss, not t, S ++ shapes, [length S] ++ SS)
  in (map (.score) poss, map (.seed) poss)

def brute_force (position: game.position)
                (turn: turn)
                (depth: i64) =
  bf_ab position turn depth

def bf_eval (depth: i64) (turn: turn) (position: game.position) =
  brute_force position turn depth

def main = bf_eval 2 true {score = 0, seed = 306}
