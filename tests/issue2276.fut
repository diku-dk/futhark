-- | file: error.fut

module type map = {
  type~ map [n]
  val member [n] : map [n] -> u64
  val from_array [u] : [u]i64 -> ?[n].map [n]
}

module mk_hashmap : map = {
  type~ map [n] =
    { level_two: [n][2 + 1]u64
    }

  def loop_body [n] [m]
                (old_keys: [n](i64, i64))
                (old_dest: *[m][2 + 1]u64) : ( [](i64, i64)
                                             , *[m][2 + 1]u64
                                             ) =
    let has_no_collisions = replicate 10 true
    let seg_has_no_collision = scan (&&) true has_no_collisions
    let new_keys =
      filter (\(_, o) -> not seg_has_no_collision[o]) old_keys
    let done = replicate n ([0, 0, 0] :> [2 + 1]u64)
    let new_dest = scatter old_dest (iota n) done
    in ( new_keys
       , new_dest
       )

  def from_array [n] (keys: [n]i64) =
    let level_one_counts = hist (+) 0i64 n keys (rep 1)
    let level_one_offsets =
      map (i64.bool <-< (!= 0)) level_one_counts
      |> scan (+) 0
      |> map2 (\f o -> if f != 0 then o - 1 else -1) level_one_counts
    let init_keys = replicate n (keys[0], level_one_offsets[0])
    let dest = replicate n (replicate (2 + 1) 0)
    let (_, done) =
      loop (old_keys, old_done) = (init_keys, dest)
      while length old_keys != 0 do
        let (new_keys, new_done) = loop_body old_keys old_done
        in (new_keys, new_done)
    in {level_two = done}

  def member [n]
             (hmap: map [n]) : u64 =
    hmap.level_two[0][0]
}

module mk_map_test (M: map)
  : {
      val test_find_all : i64 -> u64
    } = {
  def test_find_all n =
    let xs = iota n
    let h = M.from_array xs
    in M.member h
}

module hashmap = mk_hashmap
module hashmap_tests = mk_map_test hashmap

entry hashmap_find_all = hashmap_tests.test_find_all
