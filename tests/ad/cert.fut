-- Quick test that we don't crash in the presence of certificates used
-- free in vjp.

def main y = vjp (map (\x -> x / y))
