import "/futlib/complex"
import "/futlib/fft"

module c32 = complex f32
module fft = mk_fft f32

-- ==
-- entry: test_fft
-- input { [1f32, 2f32, 3f32, 4f32] [0f32, 0f32, 0f32, 0f32] }
-- output { [10f32, -2f32, -2f32, -2f32] [0f32, 2f32, 0f32, -2f32] }
entry test_fft (res: []f32) (ims: []f32) = unzip (fft.fft (zip res ims))

-- ==
-- entry: test_ifft
-- input { [10f32, -2f32, -2f32, -2f32] [0f32, 2f32, 0f32, -2f32] }
-- output { [1f32, 2f32, 3f32, 4f32] [0f32, 0f32, 0f32, 0f32] }
entry test_ifft (res: []f32) (ims: []f32) = unzip (fft.ifft (zip res ims))

-- ==
-- entry: test_fft2
-- input { [[1f32, 2f32], [3f32, 4f32]]
--         [[0f32, 0f32], [0f32, 0f32]] }
-- output { [[10f32, -2f32], [-4f32, 0f32]]
--          [[0f32, 0f32], [0f32, 0f32]] }
entry test_fft2 (res: [][]f32) (ims: [][]f32) = map2 zip res ims |> fft.fft2 |> map unzip |> unzip

-- ==
-- entry: test_ifft2
-- input { [[10f32, -2f32], [-4f32, 0f32]]
--         [[0f32, 0f32], [0f32, 0f32]] }
-- output { [[1f32, 2f32], [3f32, 4f32]]
--          [[0f32, 0f32], [0f32, 0f32]] }
entry test_ifft2 (res: [][]f32) (ims: [][]f32) = map2 zip res ims |> fft.ifft2 |> map unzip |> unzip
