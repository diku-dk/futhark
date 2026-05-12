-- Test that something defined with local is not accessible outside the module.
-- ==
-- input {} output { 1 }

def x = 1

open {local def x = 2}

def main = x
