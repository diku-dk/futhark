// Stub code for OpenCL setup.

private void OPENCL_SUCCEED(int return_code,
                        [CallerFilePath] string filePath = "",
                        [CallerLineNumber] int lineNumber = 0)
{
    OpenCLSucceed(return_code, "", filePath, lineNumber);
}

private void OPENCL_SUCCEED(ComputeErrorCode return_code,
                    [CallerFilePath] string filePath = "",
                    [CallerLineNumber] int lineNumber = 0)
{
    OpenCLSucceed((int) return_code, "", filePath, lineNumber);
}

private void OPENCL_SUCCEED(object return_code,
                    [CallerFilePath] string filePath = "",
                    [CallerLineNumber] int lineNumber = 0)
{
    OpenCLSucceed((int) return_code, "", filePath, lineNumber);
}

public struct OpenCLConfig
{
    public bool Debugging;
    public int PreferredDeviceNum;
    public string PreferredPlatform;
    public string PreferredDevice;

    public string DumpProgramTo;
    public string LoadProgramFrom;

    public int DefaultGroupSize;
    public int DefaultNumGroups;
    public int DefaultTileSize;
    public int DefaultThreshold;

    public int NumSizes;
    public string[] SizeNames;
    public string[] SizeVars;
    public int[] SizeValues;
    public string[] SizeClasses;
}

private void MemblockUnrefDevice(ref FutharkContext
 context, ref OpenCLMemblock block, string desc)
{
    if (!block.IsNull)
    {
        block.DecreaseRefs();
        if (context.DetailMemory)
        {
            Console.Error.WriteLine(String.Format(
                "Unreferencing block {0} (allocated as {1}) in {2}: {3} references remaining.",
                desc, block.Tag, "space 'device'", block.References));
        }

        if (block.References == 0)
        {
            context.CurrentMemUsageDevice -= block.Size;
            OPENCL_SUCCEED(OpenCLFree(ref context, block.Mem, block.Tag));
            block.IsNull = true;
        }

        if (context.DetailMemory)
        {
            Console.Error.WriteLine(String.Format(
                "{0} bytes freed (now allocated: {1} bytes)",
                block.Size, context.CurrentMemUsageDevice));
        }
    }
}

private void MemblockSetDevice(ref FutharkContext context,
    ref OpenCLMemblock lhs, ref OpenCLMemblock rhs, string lhs_desc)
{
    MemblockUnrefDevice(ref context, ref lhs, lhs_desc);
    rhs.IncreaseRefs();
    lhs = rhs;
}

private OpenCLMemblock MemblockAllocDevice(ref FutharkContext context, OpenCLMemblock block, long size, string desc)
{
    if (size < 0)
    {
        panic(1, String.Format("Negative allocation of {0} bytes attempted for {1} in {2}",
            size, desc));
    }

    MemblockUnrefDevice(ref context, ref block, desc);
    OPENCL_SUCCEED(OpenCLAlloc(ref context, size, desc, ref block.Mem));

    block.References = 1;
    block.IsNull = false;
    block.Size = size;
    block.Tag = desc;
    context.CurrentMemUsageDevice += size;

    if (context.DetailMemory)
    {
        Console.Error.Write(String.Format("Allocated {0} bytes for {1} in {2} (now allocated: {3} bytes)",
            size, desc, "space 'device'", Ctx.CurrentMemUsageDevice));
    }

    if (context.CurrentMemUsageDevice > context.PeakMemUsageDevice)
    {
        context.PeakMemUsageDevice = context.CurrentMemUsageDevice;
        if (context.DetailMemory)
        {
            Console.Error.Write(" (new peak).\n");
        }
    }
    else if (context.DetailMemory)
    {
        Console.Error.Write(".\n");
    }

    return block;
}


private bool FreeListFind(ref OpenCLFreeList free_list, string tag, ref long size_out, ref CLMemoryHandle mem_out)
{
    for (int i = 0; i < free_list.Capacity; i++)
    {
        if (free_list.Entries[i].Valid && free_list.Entries[i].Tag == tag)
        {
            free_list.Entries[i].Valid = false;
            size_out = free_list.Entries[i].Size;
            mem_out = free_list.Entries[i].Mem;
            free_list.Used--;
            return true;
        }
    }

    return false;
}

private bool FreeListFirst(ref OpenCLFreeList free_list, ref CLMemoryHandle mem_out)
{
    for (int i = 0; i < free_list.Capacity; i++)
    {
        if (free_list.Entries[i].Valid)
        {
            free_list.Entries[i].Valid = false;
            mem_out = free_list.Entries[i].Mem;
            free_list.Used--;
            return true;
        }
    }
    return false;
}

private ComputeErrorCode OpenCLAllocActual(ref FutharkContext context, long min_size, ref CLMemoryHandle mem)
{
    ComputeErrorCode error;
    mem = CL10.CreateBuffer(context.OpenCL.Context, ComputeMemoryFlags.ReadWrite
        , new IntPtr(min_size), IntPtr.Zero, out error);

    if (error != ComputeErrorCode.Success)
    {
        return error;
    }

    int x = 2;
    unsafe
    {
        error = CL10.EnqueueWriteBuffer(Ctx.OpenCL.Queue, mem, true, IntPtr.Zero, new IntPtr(sizeof(int)), new IntPtr(&x), 0, null, null);
    }

    return error;
}

private ComputeErrorCode OpenCLAlloc(ref FutharkContext context, long min_size, string tag, ref CLMemoryHandle mem_out)
{
    if (min_size < 0)
    {
        panic(1, "Tried to allocate a negative amount of bytes.");
    }

    min_size = (min_size < sizeof(int)) ? sizeof(int) : min_size;

    long size = 0;

    if (FreeListFind(ref context.FreeList, tag, ref size, ref mem_out))
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
        CLMemoryHandle mem = Ctx.EMPTY_MEM_HANDLE;
        if (FreeListFirst(ref context.FreeList, ref mem))
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


private ComputeErrorCode OpenCLFree(ref FutharkContext context, CLMemoryHandle mem, string tag)
{
    long size = 0;
    CLMemoryHandle existing_mem = Ctx.EMPTY_MEM_HANDLE;
    ComputeErrorCode error = ComputeErrorCode.Success;
    if (FreeListFind(ref context.FreeList, tag, ref size, ref existing_mem))
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
        FreeListInsert(ref context, size, mem, tag);
    }
    return error;
}

private void FreeListInsert(ref FutharkContext context, long size, CLMemoryHandle mem, string tag)
{
    int i = FreeListFindInvalid(ref context);
    if (i == context.FreeList.Capacity)
    {
        var cap = context.FreeList.Capacity;
        int new_capacity = cap * 2;
        Array.Resize(ref context.FreeList.Entries, new_capacity);
        for (int j = 0; j < cap; j++)
        {
            var entry = new OpenCLFreeListEntry();
            entry.Valid = false;
            context.FreeList.Entries[cap + j] = entry;
        }

        context.FreeList.Capacity *= 2;
    }

    context.FreeList.Entries[i].Valid = true;
    context.FreeList.Entries[i].Size = size;
    context.FreeList.Entries[i].Tag = tag;
    context.FreeList.Entries[i].Mem = mem;
    context.FreeList.Used++;
}

private int FreeListFindInvalid(ref FutharkContext context)
{
    int i;
    for (i = 0; i < context.FreeList.Capacity; i++)
    {
        if (!context.FreeList.Entries[i].Valid)
        {
            break;
        }
    }

    return i;
}

private class OpenCLMemblock
{
    public int References;
    public CLMemoryHandle Mem;
    public long Size;
    public string Tag;
    public bool IsNull;

    public void IncreaseRefs()
    {
        this.References += 1;
    }

    public void DecreaseRefs()
    {
        this.References -= 1;
    }
}

private OpenCLMemblock EmptyMemblock(CLMemoryHandle mem)
{
    var block = new OpenCLMemblock();
    block.Mem = mem;
    block.References = 0;
    block.Tag = "";
    block.Size = 0;
    block.IsNull = true;

    return block;
}

public struct OpenCLFreeListEntry
{
    public bool Valid;
    public CLMemoryHandle Mem;
    public long Size;
    public string Tag;
}

public struct OpenCLFreeList
{
    public OpenCLFreeListEntry[] Entries;
    public int Capacity;
    public int Used;
}


private OpenCLFreeList OpenCLFreeListInit()
{
    int CAPACITY = 30; // arbitrarily chosen
    var free_list = new OpenCLFreeList();
    free_list.Entries = Enumerable.Range(0, CAPACITY)
        .Select<int, OpenCLFreeListEntry>(_ =>
                {
                    var entry = new OpenCLFreeListEntry();
                    entry.Valid = false;
                    return entry;
                }).ToArray();

    free_list.Capacity = CAPACITY;
    free_list.Used = 0;

    return free_list;
}


private void OpenCLConfigInit(out OpenCLConfig cfg,
                      int num_sizes,
                      string[] size_names,
                      string[] size_vars,
                      int[] size_values,
                      string[] size_classes)
{
    cfg.Debugging = false;
    cfg.PreferredDeviceNum = 0;
    cfg.PreferredPlatform = "";
    cfg.PreferredDevice = "";
    cfg.DumpProgramTo = null;
    cfg.LoadProgramFrom = null;

    // The following are dummy sizes that mean the concrete defaults
    // will be set during initialisation via hardware-inspection-based
    // heuristics.
    cfg.DefaultGroupSize = 0;
    cfg.DefaultNumGroups = 0;
    cfg.DefaultTileSize = 0;
    cfg.DefaultThreshold = 0;

    cfg.NumSizes = num_sizes;
    cfg.SizeNames = size_names;
    cfg.SizeVars = size_vars;
    cfg.SizeValues = size_values;
    cfg.SizeClasses = size_classes;
}

public struct OpenCLContext {
   public CLPlatformHandle Platform;
   public CLDeviceHandle Device;
   public CLContextHandle Context;
   public CLCommandQueueHandle Queue;

   public OpenCLConfig Cfg;

   public int MaxGroupSize;
   public int MaxNumGroups;
   public int MaxTileSize;
   public int MaxThreshold;

   public int LockstepWidth;
}

public struct OpenCLDeviceOption {
    public CLPlatformHandle Platform;
    public CLDeviceHandle Device;
    public ComputeDeviceTypes DeviceType;
    public string PlatformName;
    public string DeviceName;
};

/* This function must be defined by the user.  It is invoked by
   setup_opencl() after the platform and device has been found, but
   before the program is loaded.  Its intended use is to tune
   constants based on the selected platform and device. */

private string OpenCLErrorString(int err)
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

private void OpenCLSucceed(int ret,
                   string call,
                   string file,
                   int line)
{
    if (ret != (int) ComputeErrorCode.Success)
    {
        panic(-1, "{0}:{1}: OpenCL call\n  {2}\nfailed with error code {3} ({4})\n",
              file, line, call, ret, OpenCLErrorString(ret));
    }
}

private void SetPreferredPlatform(ref OpenCLConfig cfg, string s) {
    cfg.PreferredPlatform = s;
}

private void SetPreferredDevice(ref OpenCLConfig cfg, string s)
{
    int x = 0;
    int i = 0;
    if (s[0] == '#') {
        i = 1;
        while (i < s.Length && char.IsDigit(s[i])) {
            x = x * 10 + (int) (s[i])-'0';
            i++;
        }
        // Skip trailing spaces.
        while (i < s.Length && char.IsWhiteSpace(s[i])) {
            i++;
        }
    }
    cfg.PreferredDevice = s.Substring(i);
    cfg.PreferredDeviceNum = x;
}

private string OpenCLPlatformInfo(CLPlatformHandle platform,
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

private string OpenCLDeviceInfo(CLDeviceHandle device,
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

private void OpenCLAllDeviceOptions(out OpenCLDeviceOption[] devices_out,
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
    OpenCLDeviceOption[] devices = new OpenCLDeviceOption[num_devices];

    // Loop through the platforms, getting information about their devices.
    for (int i = 0; i < num_platforms; i++) {
        CLPlatformHandle platform = all_platforms[i];
        int num_platform_devices = platform_num_devices[i];

        if (num_platform_devices == 0) {
            continue;
        }

        string platform_name = OpenCLPlatformInfo(platform, ComputePlatformInfo.Name);
        CLDeviceHandle[] platform_devices = new CLDeviceHandle[num_platform_devices];

        // Fetch all the devices.
        OPENCL_SUCCEED(CL10.GetDeviceIDs(platform, ComputeDeviceTypes.All,
                                         num_platform_devices, platform_devices, out tmp));

        IntPtr tmpptr;
        // Loop through the devices, adding them to the devices array.
        unsafe
        {
            for (int j = 0; j < num_platform_devices; j++) {
                string device_name = OpenCLDeviceInfo(platform_devices[j], ComputeDeviceInfo.Name);
                devices[num_devices_added].Platform = platform;
                devices[num_devices_added].Device = platform_devices[j];
                fixed (void* ptr = &devices[num_devices_added].DeviceType)
                {
                    OPENCL_SUCCEED(CL10.GetDeviceInfo(platform_devices[j],
                                                      ComputeDeviceInfo.Type,
                                                      new IntPtr(sizeof(ComputeDeviceTypes)),
                                                      new IntPtr(ptr),
                                                      out tmpptr));
                }
                // We don't want the structs to share memory, so copy the platform name.
                // Each device name is already unique.
                devices[num_devices_added].PlatformName = platform_name;
                devices[num_devices_added].DeviceName = device_name;
                num_devices_added++;
            }
        }
    }

    devices_out = devices;
    num_devices_out = num_devices;
}

private bool IsBlacklisted(string platform_name, string device_name)
{
    return (platform_name.Contains("Apple") &&
            device_name.Contains("Intel(R) Core(TM)"));
}

private OpenCLDeviceOption GetPreferredDevice(OpenCLConfig cfg) {
    OpenCLDeviceOption[] devices;
    int num_devices;

    OpenCLAllDeviceOptions(out devices, out num_devices);

    int num_device_matches = 0;

    for (int i = 0; i < num_devices; i++)
    {
        OpenCLDeviceOption device = devices[i];
        if (!IsBlacklisted(device.PlatformName, device.DeviceName) &&
            device.PlatformName.Contains(cfg.PreferredPlatform) &&
            device.DeviceName.Contains(cfg.PreferredDevice) &&
            num_device_matches++ == cfg.PreferredDeviceNum)
        {
            return device;
        }
    }

    panic(1, "Could not find acceptable OpenCL device.\n");
    // this is never reached
    throw new Exception();

}

private void DescribeDeviceOption(OpenCLDeviceOption device) {
    Console.Error.WriteLine("Using platform: {0}", device.PlatformName);
    Console.Error.WriteLine("Using device: {0}", device.DeviceName);
}

private ComputeProgramBuildStatus BuildOpenCLProgram(ref CLProgramHandle program, CLDeviceHandle device, string options) {
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
private CLProgramHandle SetupOpenCL(ref FutharkContext ctx,
                            string[] srcs,
                            bool required_types) {

    ComputeErrorCode error;
    CLPlatformHandle platform;
    CLDeviceHandle device;
    int MaxGroupSize;

    ctx.OpenCL.LockstepWidth = 0;

    OpenCLDeviceOption device_option = GetPreferredDevice(ctx.OpenCL.Cfg);

    if (ctx.Debugging) {
        DescribeDeviceOption(device_option);
    }

    device = device = device_option.Device;
    platform = platform = device_option.Platform;

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
                  device_option.DeviceName);
        }
    }

    unsafe
    {
        IntPtr throwaway1 = new IntPtr();
        OPENCL_SUCCEED(CL10.GetDeviceInfo(device,
                                          ComputeDeviceInfo.MaxWorkGroupSize,
                                          new IntPtr(sizeof(IntPtr)),
                                          new IntPtr(&MaxGroupSize),
                                          out throwaway1));
    }

    int MaxTileSize = (int) Math.Sqrt(MaxGroupSize);

    // Make sure this function is defined.
    PostOpenCLSetup(ref ctx, ref device_option);

    if (MaxGroupSize < ctx.OpenCL.Cfg.DefaultGroupSize) {
        Console.Error.WriteLine("Note: Device limits default group size to {0} (down from {1}).\n",
                                MaxGroupSize, ctx.OpenCL.Cfg.DefaultGroupSize);
        ctx.OpenCL.Cfg.DefaultGroupSize = MaxGroupSize;
    }

    if (MaxTileSize < ctx.OpenCL.Cfg.DefaultTileSize) {
        Console.Error.WriteLine("Note: Device limits default tile size to {0} (down from {1}).\n",
                                MaxTileSize, ctx.OpenCL.Cfg.DefaultTileSize);
        ctx.OpenCL.Cfg.DefaultTileSize = MaxTileSize;
    }

    ctx.OpenCL.MaxGroupSize = MaxGroupSize;
    ctx.OpenCL.MaxTileSize = MaxTileSize; // No limit.
    ctx.OpenCL.MaxThreshold = ctx.OpenCL.MaxNumGroups; // No limit.

    // Now we go through all the sizes, clamp them to the valid range,
    // or set them to the default.
    for (int i = 0; i < ctx.OpenCL.Cfg.NumSizes; i++) {
        string size_class = ctx.OpenCL.Cfg.SizeClasses[i];
        int size_value = ctx.OpenCL.Cfg.SizeValues[i];
        string size_name = ctx.OpenCL.Cfg.SizeNames[i];
        int max_value, default_value;
        max_value = default_value = 0;
        if (size_class.StartsWith("group_size")) {
            max_value = MaxGroupSize;
            default_value = ctx.OpenCL.Cfg.DefaultGroupSize;
        } else if (size_class.StartsWith("num_groups")) {
            max_value = MaxGroupSize; // Futhark assumes this constraint.
            default_value = ctx.OpenCL.Cfg.DefaultNumGroups;
        } else if (size_class.StartsWith("tile_size")){
            max_value = (int) Math.Sqrt(MaxGroupSize);
            default_value = ctx.OpenCL.Cfg.DefaultTileSize;
        } else if (size_class.StartsWith("threshold")) {
            max_value = 0; // No limit.
            default_value = ctx.OpenCL.Cfg.DefaultThreshold;
        } else {
            panic(1, "Unknown size class for size '{0}': {1}\n", size_name, size_class);
        }
        if (size_value == 0) {
            ctx.OpenCL.Cfg.SizeValues[i] = default_value;
        } else if (max_value > 0 && size_value > max_value) {
            Console.Error.WriteLine("Note: Device limits {0} to {1} (down from {2})",
                                    size_name, max_value, size_value);
            ctx.OpenCL.Cfg.SizeValues[i] = default_value;
        }
    }

    IntPtr[] properties = new []{
        new IntPtr((int) ComputeContextInfo.Platform),
        platform.Value,
        IntPtr.Zero
    };
    // Note that nVidia's OpenCL requires the platform property
    IntPtr _null;
    ctx.OpenCL.Context = CL10.CreateContext(properties, 1, new []{device}, null, ctx.NULL, out error);
    Debug.Assert(error == 0);

    ctx.OpenCL.Queue = CL10.CreateCommandQueue(ctx.OpenCL.Context, device, 0, out error);
    Debug.Assert(error == 0);

    if (ctx.Debugging) {
        Console.Error.WriteLine("Lockstep width: {0}\n", (int)ctx.OpenCL.LockstepWidth);
        Console.Error.WriteLine("Default group size: {0}\n", (int)ctx.OpenCL.Cfg.DefaultGroupSize);
        Console.Error.WriteLine("Default number of groups: {0}\n", (int)ctx.OpenCL.Cfg.DefaultNumGroups);
    }

    string fut_opencl_src;

    // Maybe we have to read OpenCL source from somewhere else (used for debugging).
    if (ctx.OpenCL.Cfg.LoadProgramFrom != null) {
        fut_opencl_src = File.ReadAllText(ctx.OpenCL.Cfg.LoadProgramFrom);
    } else {
        // Build the OpenCL program.  First we have to concatenate all the fragments.
        fut_opencl_src = string.Join("\n", srcs);
    }

    CLProgramHandle prog;
    error = 0;
    string[] src_ptr = new[]{fut_opencl_src};
    IntPtr[] src_size = new []{IntPtr.Zero};

    if (ctx.OpenCL.Cfg.DumpProgramTo != null) {
        File.WriteAllText(ctx.OpenCL.Cfg.DumpProgramTo, fut_opencl_src);
    }

    unsafe
    {
        prog = CL10.CreateProgramWithSource(ctx.OpenCL.Context, 1, src_ptr, src_size, out error);
    }
    Debug.Assert(error == 0);

    int compile_opts_size = 1024;

    string compile_opts = String.Format("-DLOCKSTEP_WIDTH={0} ",
                                        ctx.OpenCL.LockstepWidth);

    for (int i = 0; i < ctx.OpenCL.Cfg.NumSizes; i++) {
        compile_opts += String.Format("-D{0}={1} ",
                                      ctx.OpenCL.Cfg.SizeVars[i],
                                      ctx.OpenCL.Cfg.SizeValues[i]);
    }

    OPENCL_SUCCEED(BuildOpenCLProgram(ref prog, device, compile_opts));

    return prog;
}

private CLMemoryHandle EmptyMemHandle(CLContextHandle context)
{
    ComputeErrorCode tmp;
    var cl_mem = CL10.CreateBuffer(context, ComputeMemoryFlags.ReadWrite,
                                   IntPtr.Zero, IntPtr.Zero,
                                   out tmp);
    return cl_mem;

}

private void FutharkConfigPrintSizes()
{
    int n = FutharkGetNumSizes();
    for (int i = 0; i < n; i++)
    {
      Console.WriteLine("{0} ({1})", FutharkGetSizeName(i),
                        FutharkGetSizeClass(i));
    }
    Environment.Exit(0);
}

private void FutharkConfigSetSize(ref FutharkContextConfig config, string optarg)
{
    var name_and_value = optarg.Split('=');
    if (name_and_value.Length != 2)
    {
        panic(1, "Invalid argument for size option: {0}", optarg);
    }

    var name = name_and_value[0];
    var value = Convert.ToInt32(name_and_value[1]);
    if (!FutharkContextConfigSetSize(ref config, name, value))
    {
        panic(1, "Unknown size: {0}", name);
    }
}

private void FutharkConfigLoadTuning(ref FutharkContextConfig config, string fname)
{
    StreamReader file = new StreamReader(fname);
    String line;
    while((line = file.ReadLine()) != null)
    {
        FutharkConfigSetSize(ref config, line);
    }
    file.Close();
}
