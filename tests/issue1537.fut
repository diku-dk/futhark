module type scenario = {
  val numbers: () -> []i64

  val n_numbers: i64
}

module scenario: scenario = {
  let numbers (): []i64 = []

  let n_numbers: i64 =
    loop i = 0
    while length (numbers ()) != 0
    do i + 1
}

entry main =
  loop s = scenario.numbers ()
  for _i < scenario.n_numbers - 1
  do s ++ [1]
