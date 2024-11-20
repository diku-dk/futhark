// Only include if compiling with tensor core action
#ifdef USE_TENSOR_CORES
using namespace cute;

template<class TypeIn>
struct convert_type {
    using TypeOut = TypeIn;
};

template<>
struct convert_type<f16> {
    using TypeOut = half_t;
};

template<class ElmTypeAIn, class ElmTypeBIn, class ElmTypeCIn, class SizeM, class SizeN, class BlockSize>
struct get_mma_config {};

template<class SizeM, class SizeN, class BlockSize>
struct get_mma_config<half_t, half_t, half_t, SizeM, SizeN, BlockSize> {
//     This is how BlockSize was calculated, no need to check
    using WarpsM = Int<ceil_div(SizeM{}, 64)>;
    using WarpsN = Int<ceil_div(SizeN{}, 64)>;

// TODO: should depend on type and arch
    using MMATraits = MMA_Traits<SM80_16x8x16_F16F16F16F16_TN>;
    using MMATile = Tile<Int<16 * WarpsM{}>, Int<16 * WarpsN{}>, _16>;

    using TiledMma = TiledMMA<
    //    MMA_Atom<UniversalFMA<half_t>>,
    //    Layout<Shape<_4,_8,_1>>,
        MMA_Atom<MMATraits>,
        Layout<Shape<WarpsM,WarpsN,_1>>,
        MMATile
    >;
};


template<class SizeM, class SizeN, class BlockSize>
struct get_mma_config<half_t, half_t, float, SizeM, SizeN, BlockSize> {
//     This is how BlockSize was calculated, no need to check
    using WarpsM = Int<ceil_div(SizeM{}, 64)>;
    using WarpsN = Int<ceil_div(SizeN{}, 64)>;

// TODO: should depend on type and arch
    using MMATraits = MMA_Traits<SM80_16x8x16_F32F16F16F32_TN>;
    using MMATile = Tile<Int<16 * WarpsM{}>, Int<16 * WarpsN{}>, _16>;

    using TiledMma = TiledMMA<
    //    MMA_Atom<UniversalFMA<half_t>>,
    //    Layout<Shape<_4,_8,_1>>,
        MMA_Atom<MMATraits>,
        Layout<Shape<WarpsM,WarpsN,_1>>,
        MMATile
    >;
};


// TODO: forceinline?
template<class ElmTypeIn, class SizeY, class SizeX, class BlockSize>
FUTHARK_FUN_ATTR void futrts_copyGlobalShared(unsigned char **mem_out_p, unsigned char *global_mem, unsigned char *shared_mem, int64_t offset, ElmTypeIn, SizeY, SizeX, BlockSize)
{
    *mem_out_p = shared_mem;

    int flatThreadIdx = threadIdx.z * blockDim.y * blockDim.x + threadIdx.y * blockDim.x + threadIdx.x;

    if (flatThreadIdx < BlockSize{}) {
      using ElmType = typename convert_type<ElmTypeIn>::TypeOut;

  // TODO: check compute capability, check if transposed?
  //     using CopyOpGlobalShared = UniversalCopy<ElmType>;
  //     using CopyOpGlobalShared = UniversalCopy<uint128_t>;
      using CopyOpGlobalShared = SM80_CP_ASYNC_CACHEGLOBAL<uint128_t>;

      constexpr int elmsPerLoad = 16 / sizeof(ElmType);
      constexpr int threadsX = SizeX{} / elmsPerLoad;
      constexpr int threadsY = BlockSize{} / threadsX;

      auto s_layout = make_layout(Shape<SizeY, SizeX>{}, LayoutRight{});
      //     TODO: change if swizzling
      auto g_layout = s_layout;

      TiledCopy copy_global_shared = make_tiled_copy(Copy_Atom<CopyOpGlobalShared, ElmType>{},
          make_layout(Shape<Int<threadsY>, Int<threadsX>>{}, LayoutRight{}),
          Layout<Shape<_1,Int<elmsPerLoad>>>{}
      );

      Tensor s = make_tensor(make_smem_ptr(reinterpret_cast<ElmType *>(shared_mem)), s_layout);
  //     TODO: blockIdx.x should be arg
      Tensor g = make_tensor(make_gmem_ptr(&reinterpret_cast<ElmType *>(global_mem)[offset]), g_layout);

      ThrCopy thr_copy_global_shared = copy_global_shared.get_slice(flatThreadIdx);
      Tensor tAgA = thr_copy_global_shared.partition_S(g);
      Tensor tAsA = thr_copy_global_shared.partition_D(s);

      copy(copy_global_shared, tAgA, tAsA);

  //     TODO: use async?
      cp_async_fence();
    }

//     TODO: should ideally be just before gemm, could do in function
    cp_async_wait<0>();
    __syncthreads();
}

template<class ElmTypeAIn, class ElmTypeBIn, class ElmTypeCIn, class SizeM, class SizeN, class BlockSize, int numRegs>
FUTHARK_FUN_ATTR void futrts_copyRegistersShared(unsigned char **mem_out_p, ElmTypeCIn (&registers_mem)[numRegs], unsigned char *shared_mem, ElmTypeAIn, ElmTypeBIn, SizeM, SizeN, BlockSize)
{
    *mem_out_p = shared_mem;

    int flatThreadIdx = threadIdx.z * blockDim.y * blockDim.x + threadIdx.y * blockDim.x + threadIdx.x;

// TODO: handle non flat indices
    if (flatThreadIdx < BlockSize{}) {

        using ElmTypeA = typename convert_type<ElmTypeAIn>::TypeOut;
        using ElmTypeB = typename convert_type<ElmTypeBIn>::TypeOut;
        using ElmTypeC = typename convert_type<ElmTypeCIn>::TypeOut;

        using MMAConfig = get_mma_config<ElmTypeA, ElmTypeB, ElmTypeC, SizeM, SizeN, BlockSize>;
        using TiledMMA_ = typename MMAConfig::TiledMma;

    //    TODO: try tiledcopy instead?
        auto s_layout = make_layout(Shape<SizeM, SizeN>{}, LayoutRight{});
        TiledMMA_ tiled_mma;

        ThrMMA thr_mma = tiled_mma.get_slice(flatThreadIdx);

        auto rC_layout = partition_shape_C(thr_mma, s_layout.shape());
        Tensor tCrC = make_tensor(make_rmem_ptr(reinterpret_cast<ElmTypeC *>(registers_mem)), rC_layout);

        Tensor s = make_tensor(make_gmem_ptr(reinterpret_cast<ElmTypeC *>(shared_mem)), s_layout);
        Tensor tCgC = thr_mma.partition_C(s);

    //  TODO: take as input
        auto alpha = _1{};
        auto beta = _0{};
        axpby(alpha, tCrC, beta, tCgC);
    }
    __syncthreads();
}

template<class ElmTypeAIn, class ElmTypeBIn, class ElmTypeCIn, class SizeM, class SizeN, class SizeK, class BlockSize, int numRegs>
FUTHARK_FUN_ATTR void futrts_tensorMMM(ElmTypeCIn (*mem_out_p)[numRegs], unsigned char *A_mem, unsigned char *B_mem, ElmTypeCIn (&C_mem)[numRegs], ElmTypeAIn, ElmTypeBIn, SizeM, SizeN, SizeK, BlockSize)
{
    int flatThreadIdx = threadIdx.z * blockDim.y * blockDim.x + threadIdx.y * blockDim.x + threadIdx.x;

    using ElmTypeA = typename convert_type<ElmTypeAIn>::TypeOut;
    using ElmTypeB = typename convert_type<ElmTypeBIn>::TypeOut;
    using ElmTypeC = typename convert_type<ElmTypeCIn>::TypeOut;

    using MMAConfig = get_mma_config<ElmTypeA, ElmTypeB, ElmTypeC, SizeM, SizeN, BlockSize>;
    using TiledMMA_ = typename MMAConfig::TiledMma;

    auto sA_layout = make_layout(Shape<SizeM, SizeK>{}, LayoutRight{});
//     TODO: check this, may need K in outer dim
    auto sB_layout = make_layout(Shape<SizeN, SizeK>{}, LayoutLeft{});
    auto sC_layout = make_layout(Shape<SizeM, SizeN>{}, LayoutRight{});

    TiledMMA_ tiled_mma;

    ThrMMA thr_mma = tiled_mma.get_slice(flatThreadIdx);

    auto rC_layout = partition_shape_C(thr_mma, sC_layout.shape());
    Tensor tCrC = make_tensor(make_rmem_ptr(reinterpret_cast<ElmTypeC *>(C_mem)), rC_layout);

    Tensor sA = make_tensor(make_smem_ptr(reinterpret_cast<ElmTypeA *>(A_mem)), sA_layout);            // (BLK_M,BLK_K)
    Tensor sB = make_tensor(make_smem_ptr(reinterpret_cast<ElmTypeB *>(B_mem)), sB_layout);

    Tensor tCsA = thr_mma.partition_A(sA);
    Tensor tCsB = thr_mma.partition_B(sB);

//     TODO: use async?
//     cp_async_wait<0>();
//     __syncthreads();
    // TODO: add tDrD? try cooperative gemm
    gemm(tiled_mma, tCsA, tCsB, tCrC);
//     TODO: probably not needed since not reusing buffers
//    __syncthreads();

    for (int32_t i = 0; i < numRegs; i++)
        (*mem_out_p)[i] = C_mem[i];
}
#endif
