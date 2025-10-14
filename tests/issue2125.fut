-- Two things are necessary here:
--
--  1) not generating unnecessary versions.
--
--  2) simplifying away the slices.
--
-- ==
-- structure gpu { /If/True/SegMap 1 /If/False/SegRed 1 }

entry example_tc5 [A]
                  [B]
                  [I]
                  [J]
                  [Q]
                  (xsss: [Q][A][I]f32)
                  (ysss: [B][Q][J]f32) : [I][B][J][A]f32 =
  #[unsafe]
  map (\i ->
         -- dim 0
         map (\b ->
                -- dim 1
                map (\j ->
                       -- dim 2
                       map (\a ->
                              -- dim 3
                              map2 (*) xsss[:, a, i] ysss[b, :, j] |> f32.sum)
                           (iota A))
                    (iota J))
             (iota B))
      (iota I)
