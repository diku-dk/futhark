def index_of_first p xs =
  loop i = 0 while i < length xs && !p xs[i] do i + 1

def span p xs = let i = index_of_first p xs in (take i xs, drop i xs)

entry part1 [l] (ls: [][l]i32) =
  let blank (l: [l]i32) = null l
  in span blank ls |> \(x, y) -> (id x, tail y)
