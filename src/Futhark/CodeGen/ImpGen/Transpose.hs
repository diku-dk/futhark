-- | A cache-oblivious sequential transposition for CPU execution.
-- Generates a recursive function.
module Futhark.CodeGen.ImpGen.Transpose
  ( mapTransposeFunction,
    transposeArgs,
  )
where

import Futhark.CodeGen.ImpCode
import Futhark.IR.Prop.Types
import Futhark.Util.IntegralExp
import Prelude hiding (quot)

-- | Take well-typed arguments to the transpose function and produce
-- the actual argument list.
transposeArgs ::
  PrimType ->
  VName ->
  Count Bytes (TExp Int64) ->
  VName ->
  Count Bytes (TExp Int64) ->
  TExp Int64 ->
  TExp Int64 ->
  TExp Int64 ->
  [Arg]
transposeArgs pt destmem destoffset srcmem srcoffset num_arrays m n =
  [ MemArg destmem,
    ExpArg $ untyped $ unCount destoffset `quot` primByteSize pt,
    MemArg srcmem,
    ExpArg $ untyped $ unCount srcoffset `quot` primByteSize pt,
    ExpArg $ untyped num_arrays,
    ExpArg $ untyped m,
    ExpArg $ untyped n,
    ExpArg $ untyped (0 :: TExp Int64),
    ExpArg $ untyped m,
    ExpArg $ untyped (0 :: TExp Int64),
    ExpArg $ untyped n
  ]

-- | We need to know the name of the function we are generating, as
-- this function is recursive.
mapTransposeFunction :: Name -> PrimType -> Function op
mapTransposeFunction fname pt =
  Function
    Nothing
    []
    params
    ( mconcat
        [ dec r $ le64 re - le64 rb,
          dec c $ le64 ce - le64 cb,
          If (le64 num_arrays .==. 1) doTranspose doMapTranspose
        ]
    )
    []
    []
  where
    params =
      [ memparam destmem,
        intparam destoffset,
        memparam srcmem,
        intparam srcoffset,
        intparam num_arrays,
        intparam m,
        intparam n,
        intparam cb,
        intparam ce,
        intparam rb,
        intparam re
      ]

    memparam v = MemParam v DefaultSpace
    intparam v = ScalarParam v int64

    [ destmem,
      destoffset,
      srcmem,
      srcoffset,
      num_arrays,
      n,
      m,
      rb,
      re,
      cb,
      ce,
      r,
      c,
      i,
      j,
      val
      ] =
        zipWith
          (VName . nameFromString)
          [ "destmem",
            "destoffset",
            "srcmem",
            "srcoffset",
            "num_arrays",
            "n",
            "m",
            "rb",
            "re",
            "cb",
            "ce",
            "r",
            "c",
            "i",
            "j", -- local
            "val"
          ]
          [0 ..]

    dec v e = DeclareScalar v Nonvolatile int32 <> SetScalar v (untyped e)

    naiveTranspose =
      For j (untyped $ le64 c) $
        For i (untyped $ le64 r) $
          let i' = le64 i + le64 rb
              j' = le64 j + le64 cb
           in mconcat
                [ DeclareScalar val Nonvolatile pt,
                  Read
                    val
                    srcmem
                    (elements $ le64 srcoffset + i' * le64 m + j')
                    pt
                    DefaultSpace
                    Nonvolatile,
                  Write
                    destmem
                    (elements $ le64 destoffset + j' * le64 n + i')
                    pt
                    DefaultSpace
                    Nonvolatile
                    (var val pt)
                ]

    recArgs (cb', ce', rb', re') =
      [ MemArg destmem,
        ExpArg $ untyped $ le64 destoffset,
        MemArg srcmem,
        ExpArg $ untyped $ le64 srcoffset,
        ExpArg $ untyped $ le64 num_arrays,
        ExpArg $ untyped $ le64 m,
        ExpArg $ untyped $ le64 n,
        ExpArg $ untyped cb',
        ExpArg $ untyped ce',
        ExpArg $ untyped rb',
        ExpArg $ untyped re'
      ]

    cutoff = 64 -- arbitrary
    doTranspose =
      mconcat
        [ If
            (le64 r .<=. cutoff .&&. le64 c .<=. cutoff)
            naiveTranspose
            $ If
              (le64 r .>=. le64 c)
              ( Call
                  []
                  fname
                  ( recArgs
                      ( le64 cb,
                        le64 ce,
                        le64 rb,
                        le64 rb + (le64 r `quot` 2)
                      )
                  )
                  <> Call
                    []
                    fname
                    ( recArgs
                        ( le64 cb,
                          le64 ce,
                          le64 rb + le64 r `quot` 2,
                          le64 re
                        )
                    )
              )
              ( Call
                  []
                  fname
                  ( recArgs
                      ( le64 cb,
                        le64 cb + (le64 c `quot` 2),
                        le64 rb,
                        le64 re
                      )
                  )
                  <> Call
                    []
                    fname
                    ( recArgs
                        ( le64 cb + le64 c `quot` 2,
                          le64 ce,
                          le64 rb,
                          le64 re
                        )
                    )
              )
        ]

    doMapTranspose =
      -- In the map-transpose case, we assume that cb==rb==0, ce==m,
      -- re==n.
      For i (untyped $ le64 num_arrays) $
        Call
          []
          fname
          [ MemArg destmem,
            ExpArg $ untyped $ le64 destoffset + le64 i * le64 m * le64 n,
            MemArg srcmem,
            ExpArg $ untyped $ le64 srcoffset + le64 i * le64 m * le64 n,
            ExpArg $ untyped (1 :: TExp Int64),
            ExpArg $ untyped $ le64 m,
            ExpArg $ untyped $ le64 n,
            ExpArg $ untyped $ le64 cb,
            ExpArg $ untyped $ le64 ce,
            ExpArg $ untyped $ le64 rb,
            ExpArg $ untyped $ le64 re
          ]
