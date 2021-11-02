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
        [ dec r $ vi64 re - vi64 rb,
          dec c $ vi64 ce - vi64 cb,
          If (vi64 num_arrays .==. 1) doTranspose doMapTranspose
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
      For j (untyped $ vi64 c) $
        For i (untyped $ vi64 r) $
          let i' = vi64 i + vi64 rb
              j' = vi64 j + vi64 cb
           in mconcat
                [ DeclareScalar val Nonvolatile pt,
                  Read
                    val
                    srcmem
                    (elements $ vi64 srcoffset + i' * vi64 m + j')
                    pt
                    DefaultSpace
                    Nonvolatile,
                  Write
                    destmem
                    (elements $ vi64 destoffset + j' * vi64 n + i')
                    pt
                    DefaultSpace
                    Nonvolatile
                    (var val pt)
                ]

    recArgs (cb', ce', rb', re') =
      [ MemArg destmem,
        ExpArg $ untyped $ vi64 destoffset,
        MemArg srcmem,
        ExpArg $ untyped $ vi64 srcoffset,
        ExpArg $ untyped $ vi64 num_arrays,
        ExpArg $ untyped $ vi64 m,
        ExpArg $ untyped $ vi64 n,
        ExpArg $ untyped cb',
        ExpArg $ untyped ce',
        ExpArg $ untyped rb',
        ExpArg $ untyped re'
      ]

    cutoff = 64 -- arbitrary
    doTranspose =
      mconcat
        [ If
            (vi64 r .<=. cutoff .&&. vi64 c .<=. cutoff)
            naiveTranspose
            $ If
              (vi64 r .>=. vi64 c)
              ( Call
                  []
                  fname
                  ( recArgs
                      ( vi64 cb,
                        vi64 ce,
                        vi64 rb,
                        vi64 rb + (vi64 r `quot` 2)
                      )
                  )
                  <> Call
                    []
                    fname
                    ( recArgs
                        ( vi64 cb,
                          vi64 ce,
                          vi64 rb + vi64 r `quot` 2,
                          vi64 re
                        )
                    )
              )
              ( Call
                  []
                  fname
                  ( recArgs
                      ( vi64 cb,
                        vi64 cb + (vi64 c `quot` 2),
                        vi64 rb,
                        vi64 re
                      )
                  )
                  <> Call
                    []
                    fname
                    ( recArgs
                        ( vi64 cb + vi64 c `quot` 2,
                          vi64 ce,
                          vi64 rb,
                          vi64 re
                        )
                    )
              )
        ]

    doMapTranspose =
      -- In the map-transpose case, we assume that cb==rb==0, ce==m,
      -- re==n.
      For i (untyped $ vi64 num_arrays) $
        Call
          []
          fname
          [ MemArg destmem,
            ExpArg $ untyped $ vi64 destoffset + vi64 i * vi64 m * vi64 n,
            MemArg srcmem,
            ExpArg $ untyped $ vi64 srcoffset + vi64 i * vi64 m * vi64 n,
            ExpArg $ untyped (1 :: TExp Int64),
            ExpArg $ untyped $ vi64 m,
            ExpArg $ untyped $ vi64 n,
            ExpArg $ untyped $ vi64 cb,
            ExpArg $ untyped $ vi64 ce,
            ExpArg $ untyped $ vi64 rb,
            ExpArg $ untyped $ vi64 re
          ]
