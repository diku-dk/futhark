// Stub code for OpenCL setup.

void OPENCL_SUCCEED(int return_code,
                        [CallerFilePath] string filePath = "",
                        [CallerLineNumber] int lineNumber = 0)
{
    opencl_succeed(return_code, "", filePath, lineNumber);
}

void OPENCL_SUCCEED(ComputeErrorCode return_code,
                    [CallerFilePath] string filePath = "",
                    [CallerLineNumber] int lineNumber = 0)
{
    opencl_succeed((int) return_code, "", filePath, lineNumber);
}

void OPENCL_SUCCEED(object return_code,
                    [CallerFilePath] string filePath = "",
                    [CallerLineNumber] int lineNumber = 0)
{
    opencl_succeed((int) return_code, "", filePath, lineNumber);
}

public struct opencl_config
{
    public bool debugging;
    public int preferred_device_num;
    public string preferred_platform;
    public string preferred_device;

    public string dump_program_to;
    public string load_program_from;

    public int default_group_size;
    public int default_num_groups;
    public int default_tile_size;
    public int default_threshold;
    public int transpose_block_dim;

    public int num_sizes;
    public string[] size_names;
    public int[] size_values;
    public string[] size_classes;
}

void MemblockUnrefDevice(ref futhark_context
 context, ref opencl_memblock block, string desc)
{
    if (!block.is_null)
    {
        block.decrease_refs();
        if (context.detail_memory)
        {
            Console.Error.WriteLine(String.Format(
                "Unreferencing block {0} (allocated as {1}) in {2}: {3} references remaining.",
                desc, block.tag, "space 'device'", block.references));
        }

        if (block.references == 0)
        {
            context.cur_mem_usage_device -= block.size;
            OPENCL_SUCCEED(OpenCLFree(ref context, block.mem, block.tag));
            block.is_null = true;
        }

        if (context.detail_memory)
        {
            Console.Error.WriteLine(String.Format(
                "{0} bytes freed (now allocated: {1} bytes)",
                block.size, context.cur_mem_usage_device));
        }
    }
}

void MemblockSetDevice(ref futhark_context context,
    ref opencl_memblock lhs, ref opencl_memblock rhs, string lhs_desc)
{
    MemblockUnrefDevice(ref context, ref lhs, lhs_desc);
    rhs.increase_refs();
    lhs = rhs;
}

opencl_memblock MemblockAllocDevice(ref futhark_context context, opencl_memblock block, long size, string desc)
{
    if (size < 0)
    {
        panic(1, String.Format("Negative allocation of {0} bytes attempted for {1} in {2}",
            size, desc));
    }

    MemblockUnrefDevice(ref context, ref block, desc);
    OPENCL_SUCCEED(OpenCLAlloc(ref context, size, desc, ref block.mem));

    block.references = 1;
    block.is_null = false;
    block.size = size;
    block.tag = desc;
    context.cur_mem_usage_device += size;

    if (context.detail_memory)
    {
        Console.Error.Write(String.Format("Allocated {0} bytes for {1} in {2} (now allocated: {3} bytes)",
            size, desc, "space 'device'", ctx.cur_mem_usage_device));
    }

    if (context.cur_mem_usage_device > context.peak_mem_usage_device)
    {
        context.peak_mem_usage_device = context.cur_mem_usage_device;
        if (context.detail_memory)
        {
            Console.Error.Write(" (new peak).\n");
        }
    }
    else if (context.detail_memory)
    {
        Console.Error.Write(".\n");
    }

    return block;
}


bool free_list_find(ref opencl_free_list free_list, string tag, ref long size_out, ref CLMemoryHandle mem_out)
{
    for (int i = 0; i < free_list.capacity; i++)
    {
        if (free_list.entries[i].valid && free_list.entries[i].tag == tag)
        {
            free_list.entries[i].valid = false;
            size_out = free_list.entries[i].size;
            mem_out = free_list.entries[i].mem;
            free_list.used--;
            return true;
        }
    }

    return false;
}

bool free_list_first(ref opencl_free_list free_list, ref CLMemoryHandle mem_out)
{
    for (int i = 0; i < free_list.capacity; i++)
    {
        if (free_list.entries[i].valid)
        {
            free_list.entries[i].valid = false;
            mem_out = free_list.entries[i].mem;
            free_list.used--;
            return true;
        }
    }
    return false;
}

ComputeErrorCode OpenCLAllocActual(ref futhark_context context, long min_size, ref CLMemoryHandle mem)
{
    ComputeErrorCode error;
    mem = CL10.CreateBuffer(context.opencl.context, ComputeMemoryFlags.ReadWrite
        , new IntPtr(min_size), IntPtr.Zero, out error);

    if (error != ComputeErrorCode.Success)
    {
        return error;
    }

    int x = 2;
    unsafe
    {
        error = CL10.EnqueueWriteBuffer(ctx.opencl.queue, mem, true, IntPtr.Zero, new IntPtr(sizeof(int)), new IntPtr(&x), 0, null, null);
    }

    return error;
}

ComputeErrorCode OpenCLAlloc(ref futhark_context context, long min_size, string tag, ref CLMemoryHandle mem_out)
{
    if (min_size < 0)
    {
        panic(1, "Tried to allocate a negative amount of bytes.");
    }

    min_size = (min_size < sizeof(int)) ? sizeof(int) : min_size;

    long size = 0;

    if (free_list_find(ref context.free_list, tag, ref size, ref mem_out))
    {
        if (size >= min_size && size <= min_size * 2)
        {
            return ComputeErrorCode.Success;
        }
        else
        {
            ComputeErrorCode code1 = CL10.ReleaseMemObject(mem_out);
            if (code1 != ComputeErrorCode.Success)
            {
                return code1;
            }
        }
    }

    ComputeErrorCode error = OpenCLAllocActual(ref context, min_size, ref mem_out);
    while (error == ComputeErrorCode.MemoryObjectAllocationFailure)
    {
        CLMemoryHandle mem = ctx.EMPTY_MEM_HANDLE;
        if (free_list_first(ref context.free_list, ref mem))
        {
            error = CL10.ReleaseMemObject(mem);
            if (error != ComputeErrorCode.Success)
            {
                return error;
            }
        }
        else
        {
            break;
        }

        error = OpenCLAllocActual(ref context, min_size, ref mem_out);
    }
    return error;
}


ComputeErrorCode OpenCLFree(ref futhark_context context, CLMemoryHandle mem, string tag)
{
    long size = 0;
    CLMemoryHandle existing_mem = ctx.EMPTY_MEM_HANDLE;
    ComputeErrorCode error = ComputeErrorCode.Success;
    if (free_list_find(ref context.free_list, tag, ref size, ref existing_mem))
    {
        error = CL10.ReleaseMemObject(existing_mem);
        if (error != ComputeErrorCode.Success)
        {
            return error;
        }
    }

    if (existing_mem.Value == mem.Value)
    {
        return error;
    }

    var trash_null = new IntPtr(0);
    unsafe
    {
        error = CL10.GetMemObjectInfo(mem, ComputeMemoryInfo.Size,
            new IntPtr(sizeof(long)), new IntPtr(&size), out trash_null);
    }

    if (error == ComputeErrorCode.Success)
    {
        free_list_insert(ref context, size, mem, tag);
    }
    return error;
}

void free_list_insert(ref futhark_context context, long size, CLMemoryHandle mem, string tag)
{
    int i = free_list_find_invalid(ref context);
    if (i == context.free_list.capacity)
    {
        var cap = context.free_list.capacity;
        int new_capacity = cap * 2;
        Array.Resize(ref context.free_list.entries, new_capacity);
        for (int j = 0; j < cap; j++)
        {
            var entry = new opencl_free_list_entry();
            entry.valid = false;
            context.free_list.entries[cap + j] = entry;
        }

        context.free_list.capacity *= 2;
    }

    context.free_list.entries[i].valid = true;
    context.free_list.entries[i].size = size;
    context.free_list.entries[i].tag = tag;
    context.free_list.entries[i].mem = mem;
    context.free_list.used++;
}

int free_list_find_invalid(ref futhark_context context)
{
    int i;
    for (i = 0; i < context.free_list.capacity; i++)
    {
        if (!context.free_list.entries[i].valid)
        {
            break;
        }
    }

    return i;
}

public class opencl_memblock
{
    public int references;
    public CLMemoryHandle mem;
    public long size;
    public string tag;
    public bool is_null;

    public void increase_refs()
    {
        this.references += 1;
    }

    public void decrease_refs()
    {
        this.references -= 1;
    }
}

opencl_memblock empty_memblock(CLMemoryHandle mem)
{
    var block = new opencl_memblock();
    block.mem = mem;
    block.references = 0;
    block.tag = "";
    block.size = 0;
    block.is_null = true;

    return block;
}

public struct opencl_free_list_entry
{
    public bool valid;
    public CLMemoryHandle mem;
    public long size;
    public string tag;
}

public struct opencl_free_list
{
    public opencl_free_list_entry[] entries;
    public int capacity;
    public int used;
}


opencl_free_list opencl_free_list_init()
{
    int CAPACITY = 30; // arbitrarily chosen
    var free_list = new opencl_free_list();
    free_list.entries = Enumerable.Range(0, CAPACITY)
        .Select<int, opencl_free_list_entry>(_ =>
                {
                    var entry = new opencl_free_list_entry();
                    entry.valid = false;
                    return entry;
                }).ToArray();

    free_list.capacity = CAPACITY;
    free_list.used = 0;

    return free_list;
}


void opencl_config_init(out opencl_config cfg,
                        int num_sizes,
                        string[] size_names,
                        int[] size_values,
                        string[] size_classes)
{
    cfg.debugging = false;
    cfg.preferred_device_num = 0;
    cfg.preferred_platform = "";
    cfg.preferred_device = "";
    cfg.dump_program_to = null;
    cfg.load_program_from = null;

    cfg.default_group_size = 256;
    cfg.default_num_groups = 128;
    cfg.default_tile_size = 32;
    cfg.default_threshold = 32*1024;
    cfg.transpose_block_dim = 16;

    cfg.num_sizes = num_sizes;
    cfg.size_names = size_names;
    cfg.size_values = size_values;
    cfg.size_classes = size_classes;
}

public struct opencl_context {
   public CLPlatformHandle platform;
   public CLDeviceHandle device;
   public CLContextHandle context;
   public CLCommandQueueHandle queue;

   public opencl_config cfg;

   public int max_group_size;
   public int max_num_groups;
   public int max_tile_size;
   public int max_threshold;

   public int lockstep_width;
}

public struct opencl_device_option {
    public CLPlatformHandle platform;
    public CLDeviceHandle device;
    public ComputeDeviceTypes device_type;
    public string platform_name;
    public string device_name;
};

/* This function must be defined by the user.  It is invoked by
   setup_opencl() after the platform and device has been found, but
   before the program is loaded.  Its intended use is to tune
   constants based on the selected platform and device. */

string opencl_error_string(int err)
{
    switch ((ComputeErrorCode) err) {
        case ComputeErrorCode.Success:                                        return "Success!";
        case ComputeErrorCode.DeviceNotFound:                                 return "Device not found.";
        case ComputeErrorCode.DeviceNotAvailable:                             return "Device not available";
        case ComputeErrorCode.CompilerNotAvailable:                           return "Compiler not available";
        case ComputeErrorCode.MemoryObjectAllocationFailure:                  return "Memory object allocation failure";
        case ComputeErrorCode.OutOfResources:                                 return "Out of resources";
        case ComputeErrorCode.OutOfHostMemory:                                return "Out of host memory";
        case ComputeErrorCode.ProfilingInfoNotAvailable:                      return "Profiling information not available";
        case ComputeErrorCode.MemoryCopyOverlap:                              return "Memory copy overlap";
        case ComputeErrorCode.ImageFormatMismatch:                            return "Image format mismatch";
        case ComputeErrorCode.ImageFormatNotSupported:                        return "Image format not supported";
        case ComputeErrorCode.BuildProgramFailure:                            return "Program build failure";
        case ComputeErrorCode.MapFailure:                                     return "Map failure";
        case ComputeErrorCode.InvalidValue:                                   return "Invalid value";
        case ComputeErrorCode.InvalidDeviceType:                              return "Invalid device type";
        case ComputeErrorCode.InvalidPlatform:                                return "Invalid platform";
        case ComputeErrorCode.InvalidDevice:                                  return "Invalid device";
        case ComputeErrorCode.InvalidContext:                                 return "Invalid context";
        case ComputeErrorCode.InvalidCommandQueueFlags:                       return "Invalid queue properties";
        case ComputeErrorCode.InvalidCommandQueue:                            return "Invalid command queue";
        case ComputeErrorCode.InvalidHostPointer:                             return "Invalid host pointer";
        case ComputeErrorCode.InvalidMemoryObject:                            return "Invalid memory object";
        case ComputeErrorCode.InvalidImageFormatDescriptor:                   return "Invalid image format descriptor";
        case ComputeErrorCode.InvalidImageSize:                               return "Invalid image size";
        case ComputeErrorCode.InvalidSampler:                                 return "Invalid sampler";
        case ComputeErrorCode.InvalidBinary:                                  return "Invalid binary";
        case ComputeErrorCode.InvalidBuildOptions:                            return "Invalid build options";
        case ComputeErrorCode.InvalidProgram:                                 return "Invalid program";
        case ComputeErrorCode.InvalidProgramExecutable:                       return "Invalid program executable";
        case ComputeErrorCode.InvalidKernelName:                              return "Invalid kernel name";
        case ComputeErrorCode.InvalidKernelDefinition:                        return "Invalid kernel definition";
        case ComputeErrorCode.InvalidKernel:                                  return "Invalid kernel";
        case ComputeErrorCode.InvalidArgumentIndex:                           return "Invalid argument index";
        case ComputeErrorCode.InvalidArgumentValue:                           return "Invalid argument value";
        case ComputeErrorCode.InvalidArgumentSize:                            return "Invalid argument size";
        case ComputeErrorCode.InvalidKernelArguments:                         return "Invalid kernel arguments";
        case ComputeErrorCode.InvalidWorkDimension:                           return "Invalid work dimension";
        case ComputeErrorCode.InvalidWorkGroupSize:                           return "Invalid work group size";
        case ComputeErrorCode.InvalidWorkItemSize:                            return "Invalid work item size";
        case ComputeErrorCode.InvalidGlobalOffset:                            return "Invalid global offset";
        case ComputeErrorCode.InvalidEventWaitList:                           return "Invalid event wait list";
        case ComputeErrorCode.InvalidEvent:                                   return "Invalid event";
        case ComputeErrorCode.InvalidOperation:                               return "Invalid operation";
        case ComputeErrorCode.InvalidGLObject:                                return "Invalid OpenGL object";
        case ComputeErrorCode.InvalidBufferSize:                              return "Invalid buffer size";
        case ComputeErrorCode.InvalidMipLevel:                                return "Invalid mip-map level";
        default:                                             return "Unknown";
    }
}

void opencl_succeed(int ret,
                    string call,
                    string file,
                    int line)
{
    if (ret != (int) ComputeErrorCode.Success)
    {
        panic(-1, "{0}:{1}: OpenCL call\n  {2}\nfailed with error code {3} ({4})\n",
              file, line, call, ret, opencl_error_string(ret));
    }
}

void set_preferred_platform(ref opencl_config cfg, string s) {
    cfg.preferred_platform = s;
}

void set_preferred_device(ref opencl_config cfg, string s)
{
    int x = 0;
    int i = 0;
    if (s[0] == '#') {
        i = 1;
        while (char.IsDigit(s[i])) {
            x = x * 10 + (int) (s[i])-'0';
            i++;
        }
        // Skip trailing spaces.
        while (char.IsWhiteSpace(s[i])) {
            i++;
        }
    }
    cfg.preferred_device = s.Substring(i);
    cfg.preferred_device_num = x;
}

string opencl_platform_info(CLPlatformHandle platform,
                            ComputePlatformInfo param) {
    IntPtr req_bytes;
    IntPtr _null = new IntPtr();
    OPENCL_SUCCEED(CL10.GetPlatformInfo(platform, param, _null, _null, out req_bytes));

    byte[] info = new byte[(int) req_bytes];
    unsafe
    {
        fixed (byte* ptr = &info[0])
        {
            OPENCL_SUCCEED(CL10.GetPlatformInfo(platform, param, req_bytes, new IntPtr(ptr), out _null));
        }
    }

    return System.Text.Encoding.Default.GetString(info);
}

string opencl_device_info(CLDeviceHandle device,
                          ComputeDeviceInfo param) {
    IntPtr req_bytes;
    IntPtr _null = new IntPtr();
    OPENCL_SUCCEED(CL10.GetDeviceInfo(device, param, _null, _null, out req_bytes));

    byte[] info = new byte[(int) req_bytes];
    unsafe
    {
        fixed (byte* ptr = &info[0])
        {
            OPENCL_SUCCEED(CL10.GetDeviceInfo(device, param, req_bytes, new IntPtr(ptr), out _null));
        }
    }
    return System.Text.Encoding.Default.GetString(info);

}

void opencl_all_device_options(out opencl_device_option[] devices_out,
                               out int num_devices_out)
{
    int num_devices = 0, num_devices_added = 0;

    CLPlatformHandle[] all_platforms;
    int[] platform_num_devices;

    int num_platforms;

    // Find the number of platforms.
    OPENCL_SUCCEED(CL10.GetPlatformIDs(0, null, out num_platforms));

    // Make room for them.
    all_platforms = new CLPlatformHandle[num_platforms];
    platform_num_devices = new int[num_platforms];

    int tmp;
    // Fetch all the platforms.
    OPENCL_SUCCEED(CL10.GetPlatformIDs(num_platforms, all_platforms, out tmp));

    // Count the number of devices for each platform, as well as the
    // total number of devices.
    for (int i = 0; i < num_platforms; i++)
    {
        if (CL10.GetDeviceIDs(all_platforms[i], ComputeDeviceTypes.All,
                              0, null, out platform_num_devices[i]) == ComputeErrorCode.Success)
        {
            num_devices += platform_num_devices[i];
        }
        else
        {
            platform_num_devices[i] = 0;
        }
    }

    // Make room for all the device options.
    opencl_device_option[] devices = new opencl_device_option[num_devices];

    // Loop through the platforms, getting information about their devices.
    for (int i = 0; i < num_platforms; i++) {
        CLPlatformHandle platform = all_platforms[i];
        int num_platform_devices = platform_num_devices[i];

        if (num_platform_devices == 0) {
            continue;
        }

        string platform_name = opencl_platform_info(platform, ComputePlatformInfo.Name);
        CLDeviceHandle[] platform_devices = new CLDeviceHandle[num_platform_devices];

        // Fetch all the devices.
        OPENCL_SUCCEED(CL10.GetDeviceIDs(platform, ComputeDeviceTypes.All,
                                         num_platform_devices, platform_devices, out tmp));

        IntPtr tmpptr;
        // Loop through the devices, adding them to the devices array.
        unsafe
        {
            for (int j = 0; j < num_platform_devices; j++) {
                string device_name = opencl_device_info(platform_devices[j], ComputeDeviceInfo.Name);
                devices[num_devices_added].platform = platform;
                devices[num_devices_added].device = platform_devices[j];
                fixed (void* ptr = &devices[num_devices_added].device_type)
                {
                    OPENCL_SUCCEED(CL10.GetDeviceInfo(platform_devices[j],
                                                      ComputeDeviceInfo.Type,
                                                      new IntPtr(sizeof(ComputeDeviceTypes)),
                                                      new IntPtr(ptr),
                                                      out tmpptr));
                }
                // We don't want the structs to share memory, so copy the platform name.
                // Each device name is already unique.
                devices[num_devices_added].platform_name = platform_name;
                devices[num_devices_added].device_name = device_name;
                num_devices_added++;
            }
        }
    }

    devices_out = devices;
    num_devices_out = num_devices;
}

bool is_blacklisted(string platform_name, string device_name)
{
    return (platform_name.Contains("Apple") &&
            device_name.Contains("Intel(R) Core(TM)"));
}

opencl_device_option get_preferred_device(opencl_config cfg) {
    opencl_device_option[] devices;
    int num_devices;

    opencl_all_device_options(out devices, out num_devices);

    int num_device_matches = 0;

    for (int i = 0; i < num_devices; i++)
    {
        opencl_device_option device = devices[i];
        if (!is_blacklisted(device.platform_name, device.device_name) &&
            device.platform_name.Contains(cfg.preferred_platform) &&
            device.device_name.Contains(cfg.preferred_device) &&
            num_device_matches++ == cfg.preferred_device_num)
        {
            return device;
        }
    }

    panic(1, "Could not find acceptable OpenCL device.\n");
    // this is never reached
    throw new Exception();

}

void describe_device_option(opencl_device_option device) {
    Console.Error.WriteLine("Using platform: {0}", device.platform_name);
    Console.Error.WriteLine("Using device: {0}", device.device_name);
}

ComputeProgramBuildStatus build_opencl_program(ref CLProgramHandle program, CLDeviceHandle device, string options) {
    ComputeErrorCode ret_val = CL10.BuildProgram(program, 1, new []{device}, options, null, IntPtr.Zero);

    // Avoid termination due to CL_BUILD_PROGRAM_FAILURE
    if (ret_val != ComputeErrorCode.Success && ret_val != ComputeErrorCode.BuildProgramFailure) {
        Debug.Assert((int) ret_val == 0);
    }

    ComputeProgramBuildStatus build_status;
    unsafe
    {
        IntPtr _null = new IntPtr();
        ret_val = CL10.GetProgramBuildInfo(program,
                                           device,
                                           ComputeProgramBuildInfo.Status,
                                           new IntPtr(sizeof(int)),
                                           new IntPtr(&build_status),
                                           out _null);
    }
    Debug.Assert(ret_val == 0);

    if (build_status != ComputeProgramBuildStatus.Success) {
        char[] build_log;
        IntPtr ret_val_size;
        unsafe
        {
        ret_val = CL10.GetProgramBuildInfo(program,
                                           device,
                                           ComputeProgramBuildInfo.BuildLog,
                                           IntPtr.Zero,
                                           IntPtr.Zero,
                                           out ret_val_size);
        }
        Debug.Assert(ret_val == 0);

        build_log = new char[((int)ret_val_size)+1];
        unsafe
        {
            IntPtr _null = new IntPtr();
            fixed (char* ptr = &build_log[0])
            {
                CL10.GetProgramBuildInfo(program,
                                         device,
                                         ComputeProgramBuildInfo.BuildLog,
                                         ret_val_size,
                                         new IntPtr(ptr),
                                         out _null);
            }
        }
        Debug.Assert(ret_val == 0);

        // The spec technically does not say whether the build log is zero-terminated, so let's be careful.
        build_log[(int)ret_val_size] = '\0';
        Console.Error.Write("Build log:\n{0}\n", new string(build_log));
    }

    return build_status;
}


// We take as input several strings representing the program, because
// C does not guarantee that the compiler supports particularly large
// literals.  Notably, Visual C has a limit of 2048 characters.  The
// array must be NULL-terminated.
CLProgramHandle setup_opencl(ref futhark_context ctx,
                             string[] srcs,
                             bool required_types) {

    ComputeErrorCode error;
    CLPlatformHandle platform;
    CLDeviceHandle device;
    int max_group_size;

    ctx.opencl.lockstep_width = 0;

    opencl_device_option device_option = get_preferred_device(ctx.opencl.cfg);

    if (ctx.debugging) {
        describe_device_option(device_option);
    }

    device = device = device_option.device;
    platform = platform = device_option.platform;

    if (required_types){
        int supported;
        unsafe
        {
            IntPtr throwaway0 = new IntPtr();
            OPENCL_SUCCEED(CL10.GetDeviceInfo(device,
                                              ComputeDeviceInfo.PreferredVectorWidthDouble,
                                              new IntPtr(sizeof(IntPtr)),
                                              new IntPtr(&supported),
                                              out throwaway0));
        }
        if (supported == 0) {
            panic(1,
                  "Program uses double-precision floats, but this is not supported on chosen device: {0}\n",
                  device_option.device_name);
        }
    }

    unsafe
    {
        IntPtr throwaway1 = new IntPtr();
        OPENCL_SUCCEED(CL10.GetDeviceInfo(device,
                                          ComputeDeviceInfo.MaxWorkGroupSize,
                                          new IntPtr(sizeof(IntPtr)),
                                          new IntPtr(&max_group_size),
                                          out throwaway1));
    }

    int max_tile_size = (int) Math.Sqrt(max_group_size);

    if (max_group_size < ctx.opencl.cfg.default_group_size) {
        Console.Error.WriteLine("Note: Device limits default group size to {0} (down from {1}).\n",
                                max_group_size, ctx.opencl.cfg.default_group_size);
        ctx.opencl.cfg.default_group_size = max_group_size;
    }

    if (max_tile_size < ctx.opencl.cfg.default_tile_size) {
        Console.Error.WriteLine("Note: Device limits default tile size to {0} (down from {1}).\n",
                                max_tile_size, ctx.opencl.cfg.default_tile_size);
        ctx.opencl.cfg.default_tile_size = max_tile_size;
    }

    ctx.opencl.max_group_size = max_group_size;
    ctx.opencl.max_tile_size = max_tile_size; // No limit.
    ctx.opencl.max_threshold = ctx.opencl.max_num_groups = 0; // No limit.

    // Now we go through all the sizes, clamp them to the valid range,
    // or set them to the default.
    for (int i = 0; i < ctx.opencl.cfg.num_sizes; i++) {
        string size_class = ctx.opencl.cfg.size_classes[i];
        int size_value = ctx.opencl.cfg.size_values[i];
        string size_name = ctx.opencl.cfg.size_names[i];
        int max_value, default_value;
        max_value = default_value = 0;
        if (size_class == "group_size") {
            max_value = max_group_size;
            default_value = ctx.opencl.cfg.default_group_size;
        } else if (size_class == "num_groups") {
            max_value = max_group_size; // Futhark assumes this constraint.
            default_value = ctx.opencl.cfg.default_num_groups;
        } else if (size_class == "tile_size"){
            max_value = (int) Math.Sqrt(max_group_size);
            default_value = ctx.opencl.cfg.default_tile_size;
        } else if (size_class == "threshold") {
            max_value = 0; // No limit.
            default_value = ctx.opencl.cfg.default_threshold;
        } else {
            panic(1, "Unknown size class for size '{0}': {1}\n", size_name, size_class);
        }
        if (size_value == 0) {
            ctx.opencl.cfg.size_values[i] = default_value;
        } else if (max_value > 0 && size_value > max_value) {
            Console.Error.WriteLine("Note: Device limits {0} to {1} (down from {2})",
                                    size_name, max_value, size_value);
            ctx.opencl.cfg.size_values[i] = default_value;
        }
    }

    IntPtr[] properties = new []{
        new IntPtr((int) ComputeContextInfo.Platform),
        platform.Value,
        IntPtr.Zero
    };
    // Note that nVidia's OpenCL requires the platform property
    IntPtr _null;
    ctx.opencl.context = CL10.CreateContext(properties, 1, new []{device}, null, ctx.NULL, out error);
    Debug.Assert(error == 0);

    ctx.opencl.queue = CL10.CreateCommandQueue(ctx.opencl.context, device, 0, out error);
    Debug.Assert(error == 0);

    // Make sure this function is defined.
    post_opencl_setup(ref ctx, ref device_option);

    if (ctx.debugging) {
        Console.Error.WriteLine("Lockstep width: {0}\n", (int)ctx.opencl.lockstep_width);
        Console.Error.WriteLine("Default group size: {0}\n", (int)ctx.opencl.cfg.default_group_size);
        Console.Error.WriteLine("Default number of groups: {0}\n", (int)ctx.opencl.cfg.default_num_groups);
    }

    string fut_opencl_src;

    // Maybe we have to read OpenCL source from somewhere else (used for debugging).
    if (ctx.opencl.cfg.load_program_from != null) {
        fut_opencl_src = File.ReadAllText(ctx.opencl.cfg.load_program_from);
    } else {
        // Build the OpenCL program.  First we have to concatenate all the fragments.
        fut_opencl_src = string.Join("\n", srcs);
    }

    CLProgramHandle prog;
    error = 0;
    string[] src_ptr = new[]{fut_opencl_src};
    IntPtr[] src_size = new []{IntPtr.Zero};

    if (ctx.opencl.cfg.dump_program_to != null) {
        File.WriteAllText(ctx.opencl.cfg.dump_program_to, fut_opencl_src);
    }

    unsafe
    {
        prog = CL10.CreateProgramWithSource(ctx.opencl.context, 1, src_ptr, src_size, out error);
    }
    Debug.Assert(error == 0);

    int compile_opts_size = 1024;

    string compile_opts = String.Format("-DFUT_BLOCK_DIM={0} -DLOCKSTEP_WIDTH={1} ",
                                        ctx.opencl.cfg.transpose_block_dim,
                                        ctx.opencl.lockstep_width);

    for (int i = 0; i < ctx.opencl.cfg.num_sizes; i++) {
        compile_opts += String.Format("-D{0}={1} ",
                                      ctx.opencl.cfg.size_names[i],
                                      ctx.opencl.cfg.size_values[i]);
    }

    OPENCL_SUCCEED(build_opencl_program(ref prog, device, compile_opts));

    return prog;
}

CLMemoryHandle empty_mem_handle(CLContextHandle context)
{
    ComputeErrorCode tmp;
    var cl_mem = CL10.CreateBuffer(context, ComputeMemoryFlags.ReadWrite,
                                   IntPtr.Zero, IntPtr.Zero,
                                   out tmp);
    return cl_mem;

}

void futhark_config_print_sizes()
{
    int n = futhark_get_num_sizes();
    for (int i = 0; i < n; i++)
    {
        if (futhark_get_size_entry(i) ==  entry_point)
        {
            Console.WriteLine("{0} ({1})", futhark_get_size_name(i),
                              futhark_get_size_class(i));
        }
    }
    Environment.Exit(0);
}

void futhark_config_set_size(ref futhark_context_config config, string optarg)
{
    var name_and_value = optarg.Split('=');
    if (name_and_value.Length != 2)
    {
        panic(1, "Invalid argument for size option: {0}", optarg);
    }

    var name = name_and_value[0];
    var value = Convert.ToInt32(name_and_value[1]);
    if (!futhark_context_config_set_size(ref config, name, value))
    {
        panic(1, "Unknown size: {0}", name);
    }
}
