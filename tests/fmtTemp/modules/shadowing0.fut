-- M1.foo() calls the most recent declaration of number, due to M0.number()
-- being brought into scope of M1, overshadowing the top level definition of
-- number()
-- ==
-- input {
-- }
-- output {
--  2
-- }
