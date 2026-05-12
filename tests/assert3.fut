-- unsafe can remove assertions.
-- ==
-- compiled input { 2 } output { 1 }
-- compiled input { 3 } output { 1 }

def main (x: i32) = #[unsafe] assert (x % 2 == 0) (x / 2)
