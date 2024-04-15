-- Ignore suffixes when computing differences when possible.

def main b : [10]i64 =
  if b then iota 10 : [10]i64
  else iota 10i64 : [10]i64
