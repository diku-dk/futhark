-- ==
-- input {true} output { [1,2,3] }
-- input {false} error: hole

def main (b: bool) = if b then [1, 2, 3] else ???
