-- For some reason, having a module type of the same name as a module
-- causes the 'n' name to disappear.
-- ==

module sobol_dir = {def n : i32 = 1}

module type sobol_dir = {}

module A : {val n : i32} = sobol_dir
