-- Entry point functions are not allowed to take functions as arguments.
-- ==
-- error: Entry point functions may not be higher-order

def main (x: i32) (f: i32 -> i32, n: i32) = f x + n
