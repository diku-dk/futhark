-- The problem was that invalid size parameters were passed to the
-- 'length' function after internalisation.

def main = loop xs = [0] while length xs == 0 do xs
