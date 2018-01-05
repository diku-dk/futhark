-- This program exposed a flaw in the kernel extractor, which was
-- unable to handle identity mappings.  These rarely occur normally,
-- because the simplifier will have removed them, but they sometimes
-- occur after loop interchange.
-- ==
-- structure distributed { Kernel 1 }

let main [n] (datas: *[][n]i32) (is: []i32) =
  map (\(data: *[n]i32, old_data: *[n]i32) ->
       let (data, _) =
         loop (data, old_data) for i in [1,2,3] do
           let new_data = scatter old_data is (replicate n data[0])
           in (reshape n new_data, reshape n data)
       in data)
      (zip datas (copy datas))
