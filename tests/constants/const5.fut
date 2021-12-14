-- ==
-- structure { Screma 1 }

def big_sum = i64.sum (iota 1000000)

def main b = if b then big_sum - 1 else big_sum + 1
