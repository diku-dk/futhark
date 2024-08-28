-- ==
-- error: Causality check

entry main xs mat = map (filter (>0) xs ++) mat
