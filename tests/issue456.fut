-- This program exposed a flaw in the kernel extractor, which was
-- unable to handle identity mappings.  These rarely occur normally,
-- because the simplifier will have removed them, but they sometimes
-- occur after loop interchange.
-- ==
-- structure gpu { SegMap 1 }

def main [n] (datas: *[][n]i32) (is: []i64) =
  #[incremental_flattening(only_inner)]
  map (\(data: [n]i32, old_data: [n]i32) ->
         let (data, _) =
           loop (data: *[n]i32, old_data: *[n]i32) = (copy data, copy old_data)
           for i in [1, 2, 3] do
             let new_data = scatter old_data is (replicate n data[0])
             in (new_data : *[n]i32, data : *[n]i32)
         in data)
      (zip datas (copy datas))
