using namespace cute;

using ASmemLayout = Layout<Shape<_16, _16>, Stride<_16, _1>>;
using BSmemLayout = Layout<Shape<_16, _16>, Stride<_1, _16>>;
using TiledMma = TiledMMA<
//        MMA_Atom<UniversalFMA<half_t>>,
//        Layout<Shape<_32,_1,_1>>,
        MMA_Atom<SM80_16x8x16_F16F16F16F16_TN>,
        Layout<Shape<_1,_1,_1>>,
        Tile<_16, _16, _16>
      >;



// TODO: change
// Default implementation
FUTHARK_FUN_ATTR void futrts_copyGlobalShared(__local unsigned char **mem_out_p_0, __global unsigned char *global_mem_6286, __local unsigned char *shared_mem_6287, int64_t globalOuterDim_6281);
FUTHARK_FUN_ATTR void futrts_copyRegistersGlobal(__local unsigned char **mem_out_p_0, f16 registers_mem_6286[(int64_t) 8], __local unsigned char *global_mem_6287);
FUTHARK_FUN_ATTR void futrts_gemm_123456(f16 (*mem_out_p_0)[(int64_t) 8], __local unsigned char *A_mem_6286, __local unsigned char *B_mem_6287, f16 C_mem_6288[(int64_t) 8]);

FUTHARK_FUN_ATTR void futrts_copyGlobalShared(__local unsigned char **mem_out_p_0, __global unsigned char *global_mem_6286, __local unsigned char *shared_mem_6287, int64_t globalOuterDim_6281)
{
    __local unsigned char *mem_out_6333;

    mem_out_6333 = shared_mem_6287;
    *mem_out_p_0 = mem_out_6333;
}
FUTHARK_FUN_ATTR void futrts_copyRegistersGlobal(__local unsigned char **mem_out_p_0, f16 registers_mem_6286[(int64_t) 8], __local unsigned char *global_mem_6287)
{
    __local unsigned char *mem_out_6333;

    mem_out_6333 = global_mem_6287;
    *mem_out_p_0 = mem_out_6333;
}
FUTHARK_FUN_ATTR void futrts_gemm_123456(f16 (*mem_out_p_0)[(int64_t) 8], __local unsigned char *A_mem_6286, __local unsigned char *B_mem_6287, f16 C_mem_6288[(int64_t) 8])
{
    ASmemLayout sA_layout;
    BSmemLayout sB_layout;
    TiledMma tiled_mma;
    TiledMma::CLayout rC_layout;

    ThrMMA thr_mma = tiled_mma.get_slice(threadIdx.x);

    //   Tensor tCrC = thr_mma.make_fragment_C(tCgC);
    Tensor tCrC = make_tensor(make_rmem_ptr(reinterpret_cast<half_t *>(*mem_out_p_0)), rC_layout);

    Tensor sA = make_tensor(make_smem_ptr(reinterpret_cast<half_t *>(A_mem_6286)), sA_layout);            // (BLK_M,BLK_K)
    Tensor sB = make_tensor(make_smem_ptr(reinterpret_cast<half_t *>(B_mem_6287)), sB_layout);

    Tensor tCsA = thr_mma.partition_A(sA);
    Tensor tCsB = thr_mma.partition_B(sB);

// TODO: add tDrD?
    gemm(tiled_mma, tCsA, tCsB, tCrC);
//    gemm(tiled_mma, tCsA, tCsB, tCsB);

    f16 mem_out_6333[(int64_t) 8];

    for (int32_t i_1 = 0; i_1 < (int64_t) 8; i_1++)
        mem_out_6333[i_1] = C_mem_6288[i_1];
    for (int32_t i_2 = 0; i_2 < (int64_t) 8; i_2++)
        (*mem_out_p_0)[i_2] = mem_out_6333[i_2];
}
