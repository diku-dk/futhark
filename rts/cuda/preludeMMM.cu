using namespace cute;

template<class TypeIn>
struct convert_type {
    using TypeOut = TypeIn;
};

template<>
struct convert_type<f16> {
    using TypeOut = half_t;
};

// template<class TypeIn>
// struct get_mma_op {
//     using TypeOut = TypeIn;
// };



// TODO: forceinline?
template<class ElmTypeIn, class SizeY, class SizeX, class BlockSize>
FUTHARK_FUN_ATTR void futrts_copyGlobalShared(unsigned char **mem_out_p, unsigned char *global_mem, unsigned char *shared_mem, int64_t globalOuterDim, ElmTypeIn, SizeY, SizeX, BlockSize)
{
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
    Tensor g = make_tensor(make_gmem_ptr(&reinterpret_cast<ElmType *>(global_mem)[blockIdx.x * SizeY{} * SizeX{}]), g_layout);

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
    unsigned char *mem_out;

    mem_out = shared_mem;
    *mem_out_p = mem_out;
}

template<class ElmTypeIn, class SizeM, class SizeN, class BlockSize, int numRegs>
FUTHARK_FUN_ATTR void futrts_copyRegistersShared(unsigned char **mem_out_p, ElmTypeIn (&registers_mem)[numRegs], unsigned char *shared_mem, SizeM, SizeN, BlockSize)
{
    using ElmType = typename convert_type<ElmTypeIn>::TypeOut;
//     using ElmType = decltype(*registers_mem);

// TODO: extract magic struct
    constexpr int warpsM = ceil_div(SizeM{}, 64);
    constexpr int warpsN = ceil_div(SizeN{}, 64);

// TODO: should depend on type and arch
    using MMATraits = MMA_Traits<SM80_16x8x16_F16F16F16F16_TN>;
    using MMATile = Tile<Int<16 * warpsM>, Int<16 * warpsN>, _16>;

    using TiledMma = TiledMMA<
    //    MMA_Atom<UniversalFMA<half_t>>,
    //    Layout<Shape<_4,_8,_1>>,
        MMA_Atom<MMATraits>,
        Layout<Shape<Int<warpsM>,Int<warpsN>,_1>>,
        MMATile
    >;

//    TODO: try tiledcopy instead?
    auto s_layout = make_layout(Shape<SizeM, SizeN>{}, LayoutRight{});
    TiledMma tiled_mma;

    ThrMMA thr_mma = tiled_mma.get_slice(threadIdx.x);

    auto rC_layout = partition_shape_C(thr_mma, s_layout.shape());
    Tensor tCrC = make_tensor(make_rmem_ptr(reinterpret_cast<ElmType *>(registers_mem)), rC_layout);

    Tensor s = make_tensor(make_gmem_ptr(reinterpret_cast<ElmType *>(shared_mem)), s_layout);
    Tensor tCgC = thr_mma.partition_C(s);

//  TODO: take as input
    auto alpha = _1{};
    auto beta = _0{};
    axpby(alpha, tCrC, beta, tCgC);

    //    TODO: remove
//    for (int i = 0; i < 8; i++) {
//        reinterpret_cast<half_t *>(global_mem_6287)[i * blockDim.x + threadIdx.x] = reinterpret_cast<half_t *>(registers_mem_6286)[i];
//    }

// TODO: simplify
    unsigned char *mem_out;

    mem_out = shared_mem;
    *mem_out_p = mem_out;
}

template<class ElmTypeAIn, class ElmTypeBIn, class ElmTypeCIn, class SizeM, class SizeN, class SizeK, class BlockSize, int numRegs>
FUTHARK_FUN_ATTR void futrts_tensorMMM(ElmTypeCIn (*mem_out_p)[numRegs], unsigned char *A_mem, unsigned char *B_mem, ElmTypeCIn (&C_mem)[numRegs], ElmTypeAIn, ElmTypeBIn, SizeM, SizeN, SizeK, BlockSize)
{
    using ElmTypeA = typename convert_type<ElmTypeAIn>::TypeOut;
    using ElmTypeB = typename convert_type<ElmTypeBIn>::TypeOut;
    using ElmTypeC = typename convert_type<ElmTypeCIn>::TypeOut;

//     using ElmTypeC = decltype(*C_mem);

    constexpr int warpsM = ceil_div(SizeM{}, 64);
    constexpr int warpsN = ceil_div(SizeN{}, 64);

// TODO: should depend on type and arch
    using MMATraits = MMA_Traits<SM80_16x8x16_F16F16F16F16_TN>;
    using MMATile = Tile<Int<16 * warpsM>, Int<16 * warpsN>, _16>;

    using TiledMma = TiledMMA<
    //    MMA_Atom<UniversalFMA<half_t>>,
    //    Layout<Shape<_4,_8,_1>>,
        MMA_Atom<MMATraits>,
        Layout<Shape<Int<warpsM>,Int<warpsN>,_1>>,
        MMATile
    >;

    auto sA_layout = make_layout(Shape<SizeM, SizeK>{}, LayoutRight{});
//     TODO: check this, may need K in outer dim
    auto sB_layout = make_layout(Shape<SizeN, SizeK>{}, LayoutLeft{});
    auto sC_layout = make_layout(Shape<SizeM, SizeN>{}, LayoutRight{});

    TiledMma tiled_mma;

    ThrMMA thr_mma = tiled_mma.get_slice(threadIdx.x);

    auto rC_layout = partition_shape_C(thr_mma, sC_layout.shape());
    Tensor tCrC = make_tensor(make_rmem_ptr(reinterpret_cast<ElmTypeC *>(C_mem)), rC_layout);

    Tensor sA = make_tensor(make_smem_ptr(reinterpret_cast<ElmTypeA *>(A_mem)), sA_layout);            // (BLK_M,BLK_K)
    Tensor sB = make_tensor(make_smem_ptr(reinterpret_cast<ElmTypeB *>(B_mem)), sB_layout);

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
    ElmTypeC mem_out[numRegs];

    for (int32_t i_1 = 0; i_1 < numRegs; i_1++)
        mem_out[i_1] = C_mem[i_1];
    for (int32_t i_2 = 0; i_2 < numRegs; i_2++)
        (*mem_out_p)[i_2] = mem_out[i_2];
}