-- ==
-- property: i8_val
#[prop]
entry i8_val (i: i8) : bool =
  i > 0

-- ==
-- property: i8_arr
#[prop]
entry i8_arr (i: []i8) : bool =
    length i < 2

-- ==
-- property: u64_arr
#[prop]
entry u64_arr (i: []u64) : bool =
  length i < 2 && u64.sum i < 100

-- ==
-- property: bool_arr
#[prop]
entry bool_arr (xs: []bool): bool =
  and xs

-- ==
-- property: u8_arr_arr
#[prop]
entry u8_arr_arr (xs: [][]u8) : bool =
  length xs < 2 && u8.sum (map (\x -> u8.sum x) xs) < 100

-- ==
-- property: i16_arr_arr_arr
#[prop]
entry i16_arr_arr_arr (xss: [][][]i16) : bool =
  length xss < 2 && i16.sum (map (\xs -> i16.sum (map (\x -> i16.sum x) xs)) xss) < 100

-- ==
-- property: i8_arr_arr_arr_arr
#[prop]
entry i8_arr_arr_arr_arr (xsss: [][][][]i8) : bool =
  length xsss < 2 && i8.sum (map (\xss -> i8.sum (map (\xs -> i8.sum (map (\x -> i8.sum x) xs)) xss)) xsss) < 100

-- ==
-- property: tuple_val
#[prop]
entry tuple_val ((x: i8, y: u8)) : bool =
  x < 0 && y < 100


-- ==
-- property: record_val
#[prop]
entry record_val ({x: i8, y: u8}) : bool =
  x < 0 && y < 100

-- ==
-- property: tuple_with_arrs
#[prop]
entry tuple_with_arrs ((xs: []i8, ys: []u8)) : bool =
  -- x < 0 && y < 100
  map2 (\x y -> x < 0 && y < 100) xs ys |> and


-- ==
-- property: record_with_arrs
#[prop]
entry record_with_arrs ({xs: []i8, ys: []u8}) : bool =
  map2 (\x y -> x < 0 && y < 100) xs ys |> and

-- -- ==
-- -- property: tuple_arr
-- #[prop]
-- entry tuple_arr (xs : [](i8, u8)) : bool =
--   map (\(x, y) -> x < 0 && y < 100) xs |> and