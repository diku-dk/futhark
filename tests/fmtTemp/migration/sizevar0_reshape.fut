-- Size variables must be made available on host before use and thus block the
-- migration of any parent statements.
-- ==
-- structure gpu {
--   /If 1
--   /If/True/GPUBody 1
-- }
-- blocked.
-- blocked.
-- required on host.
-- not blocked.
-- n used as a size variable.
-- not blocked.
