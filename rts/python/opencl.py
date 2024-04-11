# Stub code for OpenCL setup.

import pyopencl as cl
import numpy as np
import sys

if cl.version.VERSION < (2015, 2):
    raise Exception(
        "Futhark requires at least PyOpenCL version 2015.2.  Installed version is %s."
        % cl.version.VERSION_TEXT
    )

TR_BLOCK_DIM = 16
TR_TILE_DIM = TR_BLOCK_DIM * 2
TR_ELEMS_PER_THREAD = 8


def parse_preferred_device(s):
    pref_num = 0
    if len(s) > 1 and s[0] == "#":
        i = 1
        while i < len(s):
            if not s[i].isdigit():
                break
            else:
                pref_num = pref_num * 10 + int(s[i])
            i += 1
        while i < len(s) and s[i].isspace():
            i += 1
        return (s[i:], pref_num)
    else:
        return (s, 0)


def get_prefered_context(
    interactive=False, platform_pref=None, device_pref=None
):
    if device_pref != None:
        (device_pref, device_num) = parse_preferred_device(device_pref)
    else:
        device_num = 0

    if interactive:
        return cl.create_some_context(interactive=True)

    def blacklisted(p, d):
        return (
            platform_pref == None
            and device_pref == None
            and p.name == "Apple"
            and d.name.find("Intel(R) Core(TM)") >= 0
        )

    def platform_ok(p):
        return not platform_pref or p.name.find(platform_pref) >= 0

    def device_ok(d):
        return not device_pref or d.name.find(device_pref) >= 0

    device_matches = 0

    for p in cl.get_platforms():
        if not platform_ok(p):
            continue
        for d in p.get_devices():
            if blacklisted(p, d) or not device_ok(d):
                continue
            if device_matches == device_num:
                return cl.Context(devices=[d])
            else:
                device_matches += 1
    raise Exception(
        "No OpenCL platform and device matching constraints found."
    )


def param_assignment(s):
    name, value = s.split("=")
    return (name, int(value))


def check_types(self, required_types):
    if "f64" in required_types:
        if (
            self.device.get_info(cl.device_info.PREFERRED_VECTOR_WIDTH_DOUBLE)
            == 0
        ):
            raise Exception(
                "Program uses double-precision floats, but this is not supported on chosen device: %s"
                % self.device.name
            )


def apply_size_heuristics(self, size_heuristics, sizes):
    for platform_name, device_type, size, valuef in size_heuristics:
        if (
            sizes[size] == None
            and self.platform.name.find(platform_name) >= 0
            and (self.device.type & device_type) == device_type
        ):
            sizes[size] = valuef(self.device)
    return sizes


def to_c_str_rep(x):
    if type(x) is bool or type(x) is np.bool_:
        if x:
            return "true"
        else:
            return "false"
    else:
        return str(x)


def initialise_opencl_object(
    self,
    program_src="",
    build_options=[],
    command_queue=None,
    interactive=False,
    platform_pref=None,
    device_pref=None,
    default_group_size=None,
    default_num_groups=None,
    default_tile_size=None,
    default_reg_tile_size=None,
    default_threshold=None,
    size_heuristics=[],
    required_types=[],
    all_sizes={},
    user_sizes={},
    constants=[],
):
    if command_queue is None:
        self.ctx = get_prefered_context(
            interactive, platform_pref, device_pref
        )
        self.queue = cl.CommandQueue(self.ctx)
    else:
        self.ctx = command_queue.context
        self.queue = command_queue
    self.device = self.queue.device
    self.platform = self.device.platform
    self.pool = cl.tools.MemoryPool(cl.tools.ImmediateAllocator(self.queue))
    device_type = self.device.type

    check_types(self, required_types)

    max_group_size = int(self.device.max_work_group_size)
    max_tile_size = int(np.sqrt(self.device.max_work_group_size))

    self.max_thread_block_size = max_group_size
    self.max_tile_size = max_tile_size
    self.max_threshold = 0
    self.max_grid_size = 0

    self.max_shared_memory = int(self.device.local_mem_size)

    # Futhark reserves 4 bytes of local memory for its own purposes.
    self.max_shared_memory -= 4

    # See comment in rts/c/opencl.h.
    if self.platform.name.find("NVIDIA CUDA") >= 0:
        self.max_shared_memory -= 12
    elif self.platform.name.find("AMD") >= 0:
        self.max_shared_memory -= 16

    self.max_registers = int(2**16)  # Not sure how to query for this.

    self.max_cache = self.device.get_info(cl.device_info.GLOBAL_MEM_CACHE_SIZE)

    if self.max_cache == 0:
        self.max_cache = 1024 * 1024

    self.free_list = {}

    self.global_failure = self.pool.allocate(np.int32().itemsize)
    cl.enqueue_fill_buffer(
        self.queue, self.global_failure, np.int32(-1), 0, np.int32().itemsize
    )
    self.global_failure_args = self.pool.allocate(
        np.int64().itemsize * (self.global_failure_args_max + 1)
    )
    self.failure_is_an_option = np.int32(0)

    if "default_group_size" in sizes:
        default_group_size = sizes["default_group_size"]
        del sizes["default_group_size"]

    if "default_num_groups" in sizes:
        default_num_groups = sizes["default_num_groups"]
        del sizes["default_num_groups"]

    if "default_tile_size" in sizes:
        default_tile_size = sizes["default_tile_size"]
        del sizes["default_tile_size"]

    if "default_reg_tile_size" in sizes:
        default_reg_tile_size = sizes["default_reg_tile_size"]
        del sizes["default_reg_tile_size"]

    if "default_threshold" in sizes:
        default_threshold = sizes["default_threshold"]
        del sizes["default_threshold"]

    default_group_size_set = default_group_size != None
    default_tile_size_set = default_tile_size != None
    default_sizes = apply_size_heuristics(
        self,
        size_heuristics,
        {
            "group_size": default_group_size,
            "tile_size": default_tile_size,
            "reg_tile_size": default_reg_tile_size,
            "num_groups": default_num_groups,
            "lockstep_width": None,
            "threshold": default_threshold,
        },
    )
    default_group_size = default_sizes["group_size"]
    default_num_groups = default_sizes["num_groups"]
    default_threshold = default_sizes["threshold"]
    default_tile_size = default_sizes["tile_size"]
    default_reg_tile_size = default_sizes["reg_tile_size"]
    lockstep_width = default_sizes["lockstep_width"]

    if default_group_size > max_group_size:
        if default_group_size_set:
            sys.stderr.write(
                "Note: Device limits group size to {} (down from {})\n".format(
                    max_tile_size, default_group_size
                )
            )
        default_group_size = max_group_size

    if default_tile_size > max_tile_size:
        if default_tile_size_set:
            sys.stderr.write(
                "Note: Device limits tile size to {} (down from {})\n".format(
                    max_tile_size, default_tile_size
                )
            )
        default_tile_size = max_tile_size

    for k, v in user_sizes.items():
        if k in all_sizes:
            all_sizes[k]["value"] = v
        else:
            raise Exception(
                "Unknown size: {}\nKnown sizes: {}".format(
                    k, " ".join(all_sizes.keys())
                )
            )

    self.sizes = {}
    for k, v in all_sizes.items():
        if v["class"] == "thread_block_size":
            max_value = max_group_size
            default_value = default_group_size
        elif v["class"] == "grid_size":
            max_value = max_group_size  # Intentional!
            default_value = default_num_groups
        elif v["class"] == "tile_size":
            max_value = max_tile_size
            default_value = default_tile_size
        elif v["class"] == "reg_tile_size":
            max_value = None
            default_value = default_reg_tile_size
        elif v["class"].startswith("threshold"):
            max_value = None
            default_value = default_threshold
        else:
            # Bespoke sizes have no limit or default.
            max_value = None
        if v["value"] == None:
            self.sizes[k] = default_value
        elif max_value != None and v["value"] > max_value:
            sys.stderr.write(
                "Note: Device limits {} to {} (down from {}\n".format(
                    k, max_value, v["value"]
                )
            )
            self.sizes[k] = max_value
        else:
            self.sizes[k] = v["value"]

    # XXX: we perform only a subset of z-encoding here.  Really, the
    # compiler should provide us with the variables to which
    # parameters are mapped.
    if len(program_src) >= 0:
        build_options += ["-DLOCKSTEP_WIDTH={}".format(lockstep_width)]

        build_options += [
            "-D{}={}".format("max_thread_block_size", max_group_size)
        ]

        build_options += [
            "-D{}={}".format(
                s.replace("z", "zz")
                .replace(".", "zi")
                .replace("#", "zh")
                .replace("'", "zq"),
                v,
            )
            for (s, v) in self.sizes.items()
        ]

        build_options += [
            "-D{}={}".format(s, to_c_str_rep(f())) for (s, f) in constants
        ]

        if self.platform.name == "Oclgrind":
            build_options += ["-DEMULATE_F16"]

        build_options += [
            f"-DTR_BLOCK_DIM={TR_BLOCK_DIM}",
            f"-DTR_TILE_DIM={TR_TILE_DIM}",
            f"-DTR_ELEMS_PER_THREAD={TR_ELEMS_PER_THREAD}",
        ]

        program = cl.Program(self.ctx, program_src).build(build_options)

        self.transpose_kernels = {
            1: {
                "default": program.map_transpose_1b,
                "low_height": program.map_transpose_1b_low_height,
                "low_width": program.map_transpose_1b_low_width,
                "small": program.map_transpose_1b_small,
                "large": program.map_transpose_1b_large,
            },
            2: {
                "default": program.map_transpose_2b,
                "low_height": program.map_transpose_2b_low_height,
                "low_width": program.map_transpose_2b_low_width,
                "small": program.map_transpose_2b_small,
                "large": program.map_transpose_2b_large,
            },
            4: {
                "default": program.map_transpose_4b,
                "low_height": program.map_transpose_4b_low_height,
                "low_width": program.map_transpose_4b_low_width,
                "small": program.map_transpose_4b_small,
                "large": program.map_transpose_4b_large,
            },
            8: {
                "default": program.map_transpose_8b,
                "low_height": program.map_transpose_8b_low_height,
                "low_width": program.map_transpose_8b_low_width,
                "small": program.map_transpose_8b_small,
                "large": program.map_transpose_8b_large,
            },
        }

        self.copy_kernels = {
            1: program.lmad_copy_1b,
            2: program.lmad_copy_2b,
            4: program.lmad_copy_4b,
            8: program.lmad_copy_8b,
        }

        return program


def opencl_alloc(self, min_size, tag):
    min_size = 1 if min_size == 0 else min_size
    assert min_size > 0
    return self.pool.allocate(min_size)


def opencl_free_all(self):
    self.pool.free_held()


def sync(self):
    failure = np.empty(1, dtype=np.int32)
    cl.enqueue_copy(self.queue, failure, self.global_failure, is_blocking=True)
    self.failure_is_an_option = np.int32(0)
    if failure[0] >= 0:
        # Reset failure information.
        cl.enqueue_fill_buffer(
            self.queue,
            self.global_failure,
            np.int32(-1),
            0,
            np.int32().itemsize,
        )

        # Read failure args.
        failure_args = np.empty(
            self.global_failure_args_max + 1, dtype=np.int64
        )
        cl.enqueue_copy(
            self.queue,
            failure_args,
            self.global_failure_args,
            is_blocking=True,
        )

        raise Exception(self.failure_msgs[failure[0]].format(*failure_args))


def map_transpose_gpu2gpu(
    self, elem_size, dst, dst_offset, src, src_offset, k, n, m
):
    kernels = self.transpose_kernels[elem_size]
    kernel = kernels["default"]
    mulx = TR_BLOCK_DIM / n
    muly = TR_BLOCK_DIM / m

    group_dims = (TR_TILE_DIM, TR_TILE_DIM // TR_ELEMS_PER_THREAD, 1)
    dims = (
        (m + TR_TILE_DIM - 1) // TR_TILE_DIM * group_dims[0],
        (n + TR_TILE_DIM - 1) // TR_TILE_DIM * group_dims[1],
        k,
    )

    k32 = np.int32(k)
    n32 = np.int32(n)
    m32 = np.int32(m)
    mulx32 = np.int32(mulx)
    muly32 = np.int32(muly)

    kernel.set_args(
        cl.LocalMemory(TR_TILE_DIM * (TR_TILE_DIM + 1) * elem_size),
        dst,
        dst_offset,
        src,
        src_offset,
        k32,
        m32,
        n32,
        mulx32,
        muly32,
        np.int32(0),
        np.int32(0),
    )
    cl.enqueue_nd_range_kernel(self.queue, kernel, dims, group_dims)


def copy_elements_gpu2gpu(
    self,
    elem_size,
    dst,
    dst_offset,
    dst_strides,
    src,
    src_offset,
    src_strides,
    shape,
):
    r = len(shape)
    if r > 8:
        raise Exception(
            "Futhark runtime limitation:\nCannot copy array of greater than rank 8.\n"
        )

    n = np.prod(shape)
    zero = np.int64(0)
    layout_args = [None] * (8 * 3)
    for i in range(8):
        if i < r:
            layout_args[i * 3 + 0] = shape[i]
            layout_args[i * 3 + 1] = dst_strides[i]
            layout_args[i * 3 + 2] = src_strides[i]
        else:
            layout_args[i * 3 + 0] = zero
            layout_args[i * 3 + 1] = zero
            layout_args[i * 3 + 2] = zero

    kernel = self.copy_kernels[elem_size]
    kernel.set_args(
        cl.LocalMemory(1),
        dst,
        dst_offset,
        src,
        src_offset,
        n,
        np.int32(r),
        *layout_args,
    )
    w = 256
    dims = ((n + w - 1) // w * w,)
    group_dims = (w,)
    cl.enqueue_nd_range_kernel(self.queue, kernel, dims, group_dims)


def lmad_copy_gpu2gpu(
    self, pt, dst, dst_offset, dst_strides, src, src_offset, src_strides, shape
):
    elem_size = ct.sizeof(pt)
    nbytes = np.prod(shape) * elem_size
    if nbytes == 0:
        return None
    if lmad_memcpyable(dst_strides, src_strides, shape):
        cl.enqueue_copy(
            self.queue,
            dst,
            src,
            dst_offset=dst_offset * elem_size,
            src_offset=src_offset * elem_size,
            byte_count=nbytes,
        )
    else:
        tr = lmad_map_tr(dst_strides, src_strides, shape)
        if tr is not None:
            (k, n, m) = tr
            map_transpose_gpu2gpu(
                self, elem_size, dst, dst_offset, src, src_offset, k, m, n
            )
        else:
            copy_elements_gpu2gpu(
                self,
                elem_size,
                dst,
                dst_offset,
                dst_strides,
                src,
                src_offset,
                src_strides,
                shape,
            )
