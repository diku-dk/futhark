using namespace cute;

template<class TypeIn>
struct convert_type {
    using TypeOut = TypeIn;
};

template<>
struct convert_type<f16> {
    using TypeOut = half_t;
};

template<class ElmTypeAIn, class ElmTypeBIn, class ElmTypeCIn, class SizeM, class SizeN, class WarpsM, class WarpsN>
struct get_mma_config {};

// TODO: add fallback FMA?

template<class SizeM, class SizeN, class WarpsM, class WarpsN>
struct get_mma_config<half_t, half_t, half_t, SizeM, SizeN, WarpsM, WarpsN> {
    // TODO: should depend on type and arch
    using MMATraits = MMA_Traits<SM80_16x8x16_F16F16F16F16_TN>;

    using ACopyOpSharedRegisters = SM75_U32x4_LDSM_N;
    using BCopyOpSharedRegisters = SM75_U16x8_LDSM_T;
    using MMATile = Tile<Int<16 * WarpsM{}>, Int<16 * WarpsN{}>, _16>;
    using TiledMMA = TiledMMA<
        MMA_Atom<MMATraits>,
        Layout<Shape<WarpsM,WarpsN,_1>>,
        MMATile
    >;
};

template<class SizeM, class SizeN, class WarpsM, class WarpsN>
struct get_mma_config<half_t, half_t, float, SizeM, SizeN, WarpsM, WarpsN>{
    // TODO: should depend on type and arch
    using MMATraits = MMA_Traits<SM80_16x8x16_F32F16F16F32_TN>;

    using ACopyOpSharedRegisters = SM75_U32x4_LDSM_N;
    using BCopyOpSharedRegisters = SM75_U16x8_LDSM_T;
    using MMATile = Tile<Int<16 * WarpsM{}>, Int<16 * WarpsN{}>, _16>;
    using TiledMMA = TiledMMA<
        MMA_Atom<MMATraits>,
        Layout<Shape<WarpsM,WarpsN,_1>>,
        MMATile
    >;
};

template<class SizeY, class SizeX, class Swizzle, class Majorness>
struct get_layout_config {};

template<class SizeY, class SizeX>
struct get_layout_config<SizeY, SizeX, _1, LayoutRight>{
    using SharedLayout = ComposedLayout<Swizzle<3, 3>, _0, Layout<Shape<SizeY, SizeX>, Stride<SizeX, _1>>>;
};

template<class SizeY, class SizeX>
struct get_layout_config<SizeY, SizeX, _0, LayoutRight>{
    using SharedLayout = Layout<Shape<SizeY, SizeX>, Stride<SizeX, _1>>;
};

template<class SizeY, class SizeX>
struct get_layout_config<SizeY, SizeX, _1, LayoutLeft>{
    using SharedLayout = ComposedLayout<Swizzle<3, 3>, _0, Layout<Shape<SizeY, SizeX>, Stride<_1, SizeY>>>;
};

template<class SizeY, class SizeX>
struct get_layout_config<SizeY, SizeX, _0, LayoutLeft>{
    using SharedLayout = Layout<Shape<SizeY, SizeX>, Stride<_1, SizeY>>;
};


// TODO: forceinline?
template<class ElmTypeIn, class SizeY, class SizeX, class WarpsM, class WarpsN>
FUTHARK_FUN_ATTR void futrts_copyGlobalShared(unsigned char **mem_out_p, unsigned char *global_mem, unsigned char *shared_mem, int64_t offset, ElmTypeIn, SizeY, SizeX, WarpsM, WarpsN)
{
    *mem_out_p = shared_mem;

    int flatThreadIdx = threadIdx.z * blockDim.y * blockDim.x + threadIdx.y * blockDim.x + threadIdx.x;

    if (flatThreadIdx < WarpsM{} * WarpsN{} * 32) {
      using ElmType = typename convert_type<ElmTypeIn>::TypeOut;

  // TODO: check compute capability, check if transposed?
  //     using CopyOpGlobalShared = UniversalCopy<ElmType>;
  //     using CopyOpGlobalShared = UniversalCopy<uint128_t>;
      using CopyOpGlobalShared = SM80_CP_ASYNC_CACHEGLOBAL<uint128_t>;

      constexpr int elmsPerLoad = 16 / sizeof(ElmType);
      constexpr int threadsX = SizeX{} / elmsPerLoad;
      constexpr int threadsY = (WarpsM{} * WarpsN{} * 32) / threadsX;

      auto s_layout = composition(Swizzle<3, 3>{}, make_layout(Shape<SizeY, SizeX>{}, LayoutRight{}));
      auto g_layout = make_layout(Shape<SizeY, SizeX>{}, LayoutRight{});

      TiledCopy copy_global_shared = make_tiled_copy(Copy_Atom<CopyOpGlobalShared, ElmType>{},
          make_layout(Shape<Int<threadsY>, Int<threadsX>>{}, LayoutRight{}),
          Layout<Shape<_1,Int<elmsPerLoad>>>{}
      );

      Tensor s = make_tensor(make_smem_ptr(reinterpret_cast<ElmType *>(shared_mem)), s_layout);
      Tensor g = make_tensor(make_gmem_ptr(&reinterpret_cast<ElmType *>(global_mem)[offset]), g_layout);

      ThrCopy thr_copy_global_shared = copy_global_shared.get_slice(flatThreadIdx);
      Tensor tAgA = thr_copy_global_shared.partition_S(g);
      Tensor tAsA = thr_copy_global_shared.partition_D(s);

      copy(copy_global_shared, tAgA, tAsA);

      // TODO: use async?
      cp_async_fence();
    }

    // TODO: should ideally be just before gemm, could do in function
    cp_async_wait<0>();
    __syncthreads();
}

template<class ElmTypeAIn, class ElmTypeBIn, class ElmTypeCIn, class SizeM, class SizeN, class WarpsM, class WarpsN, int numRegs>
FUTHARK_FUN_ATTR void futrts_copyRegistersShared(unsigned char **mem_out_p, ElmTypeCIn (&registers_mem)[numRegs], unsigned char *shared_mem, ElmTypeAIn, ElmTypeBIn, SizeM, SizeN, WarpsM, WarpsN)
{
    *mem_out_p = shared_mem;

    int flatThreadIdx = threadIdx.z * blockDim.y * blockDim.x + threadIdx.y * blockDim.x + threadIdx.x;

    if (flatThreadIdx < WarpsM{} * WarpsN{} * 32) {

        using ElmTypeA = typename convert_type<ElmTypeAIn>::TypeOut;
        using ElmTypeB = typename convert_type<ElmTypeBIn>::TypeOut;
        using ElmTypeC = typename convert_type<ElmTypeCIn>::TypeOut;

        using MMAConfig = get_mma_config<ElmTypeA, ElmTypeB, ElmTypeC, SizeM, SizeN, WarpsM, WarpsN>;
        typename MMAConfig::TiledMMA tiled_mma;

        auto s_layout = make_layout(Shape<SizeM, SizeN>{}, LayoutRight{});

        ThrMMA thr_mma = tiled_mma.get_slice(flatThreadIdx);

        auto rC_layout = partition_shape_C(thr_mma, s_layout.shape());
        Tensor tCrC = make_tensor(make_rmem_ptr(reinterpret_cast<ElmTypeC *>(registers_mem)), rC_layout);

        Tensor s = make_tensor(make_gmem_ptr(reinterpret_cast<ElmTypeC *>(shared_mem)), s_layout);
        Tensor tCsC = thr_mma.partition_C(s);

        //  TODO: take as input, or just use copy instead?
//         auto alpha = _1{};
//         auto beta = _0{};
//         axpby(alpha, tCrC, beta, tCsC);
        copy(AutoVectorizingCopy{}, tCrC, tCsC);
    }
    __syncthreads();
}

template<class ElmTypeAIn, class ElmTypeBIn, class ElmTypeCIn, class SizeM, class SizeN, class SizeK, class WarpsM, class WarpsN, class ASwizzled, class BSwizzled, int numRegs>
FUTHARK_FUN_ATTR void futrts_tensorMMM(ElmTypeCIn (*mem_out_p)[numRegs], unsigned char *A_mem, unsigned char *B_mem, ElmTypeCIn (&C_mem)[numRegs], ElmTypeAIn, ElmTypeBIn, SizeM, SizeN, SizeK, WarpsM, WarpsN, ASwizzled, BSwizzled)
{
    int flatThreadIdx = threadIdx.z * blockDim.y * blockDim.x + threadIdx.y * blockDim.x + threadIdx.x;

    using ElmTypeA = typename convert_type<ElmTypeAIn>::TypeOut;
    using ElmTypeB = typename convert_type<ElmTypeBIn>::TypeOut;
    using ElmTypeC = typename convert_type<ElmTypeCIn>::TypeOut;

    using MMAConfig = get_mma_config<ElmTypeA, ElmTypeB, ElmTypeC, SizeM, SizeN, WarpsM, WarpsN>;
    typename MMAConfig::TiledMMA tiled_mma;

    using ALayoutConfig = get_layout_config<SizeM, SizeK, ASwizzled, LayoutRight>;
    using BLayoutConfig = get_layout_config<SizeN, SizeK, BSwizzled, LayoutLeft>;
    typename ALayoutConfig::SharedLayout sA_layout;
    typename BLayoutConfig::SharedLayout sB_layout;

    auto sC_layout = make_layout(Shape<SizeM, SizeN>{}, LayoutRight{});

    ThrMMA thr_mma = tiled_mma.get_slice(flatThreadIdx);

    auto rC_layout = partition_shape_C(thr_mma, sC_layout.shape());
    Tensor tCrC = make_tensor(make_rmem_ptr(reinterpret_cast<ElmTypeC *>(C_mem)), rC_layout);

    Tensor sA = make_tensor(make_smem_ptr(reinterpret_cast<ElmTypeA *>(A_mem)), sA_layout);
    Tensor sB = make_tensor(make_smem_ptr(reinterpret_cast<ElmTypeB *>(B_mem)), sB_layout);

    TiledCopy smem_tiled_copy_A = make_tiled_copy_A(Copy_Atom<typename MMAConfig::ACopyOpSharedRegisters, ElmTypeA>{}, tiled_mma);
    TiledCopy smem_tiled_copy_B = make_tiled_copy_B(Copy_Atom<typename MMAConfig::BCopyOpSharedRegisters, ElmTypeB>{}, tiled_mma);

    // Create register tensors for the MMA to operate on
    Tensor tCrA  = thr_mma.partition_fragment_A(sA);
    Tensor tCrB  = thr_mma.partition_fragment_B(sB);

    auto smem_thr_copy_A   = smem_tiled_copy_A.get_thread_slice(threadIdx.x);
    Tensor tCsA            = smem_thr_copy_A.partition_S(sA);
    Tensor tCrA_copy_view  = smem_thr_copy_A.retile_D(tCrA);

    auto smem_thr_copy_B   = smem_tiled_copy_B.get_thread_slice(threadIdx.x);
    Tensor tCsB            = smem_thr_copy_B.partition_S(sB);
    Tensor tCrB_copy_view  = smem_thr_copy_B.retile_D(tCrB);

    // TODO: use async better? add tDrD? try cooperative gemm

    // Inner loop
    constexpr int K_BLOCK_MAX = size<2>(tCrA);
    CUTE_UNROLL
    for (int k_block = 0; k_block < K_BLOCK_MAX; ++k_block)
    {
        // Copy shared -> registers
        copy(smem_tiled_copy_A, tCsA(_,_,k_block), tCrA_copy_view(_,_,k_block));
        copy(smem_tiled_copy_B, tCsB(_,_,k_block), tCrB_copy_view(_,_,k_block));

        // GEMM on k_block in registers
        gemm(tiled_mma, tCrA(_,_,k_block), tCrB(_,_,k_block), tCrC);
    }

    for (int32_t i = 0; i < numRegs; i++)
        (*mem_out_p)[i] = C_mem[i];
}
