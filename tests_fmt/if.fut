def a = if true then 1 else 2

def b =
  if true
  then 1
       + 3 + 5
  else 2

def c =
  if true
  then 1
  else if true
  then 2
  else 3

def d =
  if true
  then -- foo
       true
  else -- bar
       true
