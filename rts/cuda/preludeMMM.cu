using namespace cute;

using AGmemLayout = Layout<Shape<_16, _16>, Stride<_16, _1>>;
using ASmemLayout = Layout<Shape<_16, _16>, Stride<_16, _1>>;
using BSmemLayout = Layout<Shape<_16, _16>, Stride<_1, _16>>;
using TiledMma = TiledMMA<
//    MMA_Atom<UniversalFMA<half_t>>,
//    Layout<Shape<_4,_8,_1>>,
    MMA_Atom<SM80_16x8x16_F16F16F16F16_TN>,
    Layout<Shape<_1,_1,_1>>,
    Tile<_16, _16, _16>
>;
// TODO: get from TiledMma
using RCLayout = Layout<Shape<Shape<_2, _2>, _1, _2>, Stride<Stride<_1, _2>, _1, _4>>;
//using RCLayout = Layout<Shape<_1, _4, _2>>;

// TODO: use vectorized or async
// using CopyOpGlobalShared = UniversalCopy<half_t>;
// using CopyOpGlobalShared = UniversalCopy<uint128_t>;
using CopyOpGlobalShared = SM80_CP_ASYNC_CACHEGLOBAL<uint128_t>;
//using TiledCopy_ = TiledCopy<Copy_Atom<CopyOpGlobalShared, half_t>,
//                            Layout<
//                                Shape<_16,_2>,
//                                Stride<_8,_1>
//                            >,
//                            Layout<Shape<_1,_8>>
//                    >


// TODO: change
// Default implementation
FUTHARK_FUN_ATTR void futrts_copyGlobalShared(__local unsigned char **mem_out_p_0, __global unsigned char *global_mem_6286, __local unsigned char *shared_mem_6287, int64_t globalOuterDim_6281);
FUTHARK_FUN_ATTR void futrts_copyRegistersGlobal(__local unsigned char **mem_out_p_0, f16 registers_mem_6286[(int64_t) 8], __local unsigned char *global_mem_6287);
FUTHARK_FUN_ATTR void futrts_gemm_123456(f16 (*mem_out_p_0)[(int64_t) 8], __local unsigned char *A_mem_6286, __local unsigned char *B_mem_6287, f16 C_mem_6288[(int64_t) 8]);

FUTHARK_FUN_ATTR void futrts_copyGlobalShared(__local unsigned char **mem_out_p_0, __global unsigned char *global_mem_6286, __local unsigned char *shared_mem_6287, int64_t globalOuterDim_6281)
{
//    TODO: should be different for B?
    ASmemLayout s_layout;
    ASmemLayout g_layout;
    TiledCopy copy_global_shared = make_tiled_copy(Copy_Atom<CopyOpGlobalShared, half_t>{},
        Layout<
            Shape<_16,_2>,
            Stride<_2,_1>
        >{},
        Layout<Shape<_1,_8>>{}
    );

    Tensor s = make_tensor(make_smem_ptr(reinterpret_cast<half_t *>(shared_mem_6287)), s_layout);
    Tensor g = make_tensor(make_gmem_ptr(&reinterpret_cast<half_t *>(global_mem_6286)[blockIdx.x * 16 * 16]), g_layout);

    ThrCopy thr_copy_global_shared = copy_global_shared.get_slice(threadIdx.x);
    Tensor tAgA = thr_copy_global_shared.partition_S(g);
    Tensor tAsA = thr_copy_global_shared.partition_D(s);

    copy(copy_global_shared, tAgA, tAsA);

//     TODO: use async?
    cp_async_fence();

    //    TODO: remove
//    for (int i = 0; i < 8; i++) {
//        reinterpret_cast<half_t *>(shared_mem_6287)[i * blockDim.x + threadIdx.x] = reinterpret_cast<half_t *>(global_mem_6286)[blockIdx.x * 16 * 16 + i * blockDim.x + threadIdx.x];
//    }

// TODO: simplify
    __local unsigned char *mem_out_6333;

    mem_out_6333 = shared_mem_6287;
    *mem_out_p_0 = mem_out_6333;
}
FUTHARK_FUN_ATTR void futrts_copyRegistersGlobal(__local unsigned char **mem_out_p_0, f16 registers_mem_6286[(int64_t) 8], __local unsigned char *global_mem_6287)
{
//    TODO: try tiledcopy instead?
    ASmemLayout g_layout;
    TiledMma tiled_mma;
    RCLayout rC_layout;

    ThrMMA thr_mma = tiled_mma.get_slice(threadIdx.x);

    Tensor g = make_tensor(make_gmem_ptr(reinterpret_cast<half_t *>(global_mem_6287)), g_layout);
    Tensor tCrC = make_tensor(make_rmem_ptr(reinterpret_cast<half_t *>(registers_mem_6286)), rC_layout);

    Tensor tCgC = thr_mma.partition_C(g);
//    TODO: use this?
//    Tensor tCrC = thr_mma.make_fragment_C(tCgC);

//  TODO: take as input
    auto alpha = _1{};
    auto beta = _0{};
    axpby(alpha, tCrC, beta, tCgC);

    //    TODO: remove
//    for (int i = 0; i < 8; i++) {
//        reinterpret_cast<half_t *>(global_mem_6287)[i * blockDim.x + threadIdx.x] = reinterpret_cast<half_t *>(registers_mem_6286)[i];
//    }

// TODO: simplify
    __local unsigned char *mem_out_6333;

    mem_out_6333 = global_mem_6287;
    *mem_out_p_0 = mem_out_6333;
}
FUTHARK_FUN_ATTR void futrts_gemm_123456(f16 (*mem_out_p_0)[(int64_t) 8], __local unsigned char *A_mem_6286, __local unsigned char *B_mem_6287, f16 C_mem_6288[(int64_t) 8])
{
    ASmemLayout sA_layout;
    BSmemLayout sB_layout;
    TiledMma tiled_mma;
    RCLayout rC_layout;

    ThrMMA thr_mma = tiled_mma.get_slice(threadIdx.x);

    Tensor tCrC = make_tensor(make_rmem_ptr(reinterpret_cast<half_t *>(C_mem_6288)), rC_layout);

//    Tensor gC = make_tensor(make_rmem_ptr(reinterpret_cast<half_t *>(*mem_out_p_0)), rC_layout);
//    Tensor tCrC = thr_mma.make_fragment_C(tCgC);
//    Tensor tCrC = thr_mma.partition_C(gC);

    Tensor sA = make_tensor(make_smem_ptr(reinterpret_cast<half_t *>(A_mem_6286)), sA_layout);            // (BLK_M,BLK_K)
    Tensor sB = make_tensor(make_smem_ptr(reinterpret_cast<half_t *>(B_mem_6287)), sB_layout);

    Tensor tCsA = thr_mma.partition_A(sA);
    Tensor tCsB = thr_mma.partition_B(sB);

//     TODO: use async?
    cp_async_wait<0>();
    __syncthreads();
    // TODO: add tDrD?
    gemm(tiled_mma, tCsA, tCsB, tCrC);
//     TODO: probably not needed since not reusing buffers
//    __syncthreads();

//    for (int i = 0; i < 8; i++) {
//        int m = (i * blockDim.x + threadIdx.x) / 16;
//        int n = (i * blockDim.x + threadIdx.x) % 16;
//        for (int k = 0; k < 16; k++)
//        {
//            half_t a_elm = reinterpret_cast<half_t *>(A_mem_6286)[m * 16 + k];
//            half_t b_elm = reinterpret_cast<half_t *>(B_mem_6287)[k * 16 + n];
//            reinterpret_cast<half_t *>(C_mem_6288)[i] += a_elm * b_elm;
//        }
//    }

// TODO: remove
//     for (int i = 0; i < 8; i++) {
//         reinterpret_cast<half_t *>(*mem_out_p_0)[i] = reinterpret_cast<half_t *>(B_mem_6287)[i * blockDim.x + threadIdx.x];
//     }

// TODO: simplify
    f16 mem_out_6333[(int64_t) 8];

    for (int32_t i_1 = 0; i_1 < (int64_t) 8; i_1++)
        mem_out_6333[i_1] = C_mem_6288[i_1];
    for (int32_t i_2 = 0; i_2 < (int64_t) 8; i_2++)
        (*mem_out_p_0)[i_2] = mem_out_6333[i_2];
}