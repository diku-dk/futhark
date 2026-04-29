-- Test entry points that return pairs

entry get_pair (x: i32) = (x + 1, x + 2)

entry use_pair (x: i32, y: i32) = x + y

entry get_nested_pair (x: i32) = ((x + 1, x + 2), (x + 3))
