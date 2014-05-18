fun char digitChar (int x) =
    if x == 0 then '0'
    else if x == 1 then '1'
    else if x == 2 then '2'
    else if x == 3 then '3'
    else if x == 4 then '4'
    else if x == 5 then '5'
    else if x == 6 then '6'
    else if x == 7 then '7'
    else if x == 8 then '8'
    else '9'

fun [char] itoa (int x) = concat(if (x / 10) == 0 then "" else itoa(x / 10), [digitChar (x % 10)])

fun [char] verse(int i) =
    let num = itoa(i) in
    concat(concat(concat(concat(concat(num, " bottles of beer on the wall, "), num), " bottles of beer.\nTake one down, pass it around, "), num), " bottles of beer on the wall.\n")

fun [char] main() =
  let n = 99 in
  let lastverse = "One bottle of beer on the wall, one bottle of beer.\nTake one down, pass it around, one bottle of beer on the wall.\n" in
  loop (song="") = for i < n-1 do
    concat(song, verse(n-i)) in
  concat(song, lastverse)
