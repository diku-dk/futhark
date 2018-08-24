private Tuple<byte[], long[]> createTuple_byte(CLMemoryHandle mem, CLCommandQueueHandle queue,
                                       int nbytes, long[] shape)
{
    var byteArray = new byte[1];
    if (nbytes > 0)
    {
        byteArray = new byte[nbytes / sizeof(byte)];
    }
    unsafe
    {
        fixed(void* ptr = &byteArray[0])
        {
            CL10.EnqueueReadBuffer(queue, mem, true,
                                   new IntPtr(0), new IntPtr(nbytes), new IntPtr(ptr),
                                   0, null, null
                                   );
        }
    }
    return Tuple.Create(byteArray, shape);
}

private Tuple<ushort[], long[]> createTuple_ushort(CLMemoryHandle mem, CLCommandQueueHandle queue,
                                           int nbytes, long[] shape)
{
    var byteArray = new ushort[1];
    if (nbytes > 0)
    {
        byteArray = new ushort[nbytes / sizeof(ushort)];
    }
    unsafe
    {
        fixed(void* ptr = &byteArray[0])
        {
            CL10.EnqueueReadBuffer(queue, mem, true,
                                   new IntPtr(0), new IntPtr(nbytes), new IntPtr(ptr),
                                   0, null, null
                                   );
        }
    }
    return Tuple.Create(byteArray, shape);
}

private Tuple<uint[], long[]> createTuple_uint(CLMemoryHandle mem, CLCommandQueueHandle queue,
                                   int nbytes, long[] shape)
{
    var byteArray = new uint[1];
    if (nbytes > 0)
    {
        byteArray = new uint[nbytes / sizeof(uint)];
    }
    unsafe
    {
        fixed(void* ptr = &byteArray[0])
        {
            CL10.EnqueueReadBuffer(queue, mem, true,
                                   new IntPtr(0), new IntPtr(nbytes), new IntPtr(ptr),
                                   0, null, null
                                   );
        }
    }
    return Tuple.Create(byteArray, shape);
}

private Tuple<ulong[], long[]> createTuple_ulong(CLMemoryHandle mem, CLCommandQueueHandle queue,
                                 int nbytes, long[] shape)
{
    var byteArray = new ulong[1];
    if (nbytes > 0)
    {
        byteArray = new ulong[nbytes / sizeof(ulong)];
    }
    unsafe
    {
        fixed(void* ptr = &byteArray[0])
        {
            CL10.EnqueueReadBuffer(queue, mem, true,
                                   new IntPtr(0), new IntPtr(nbytes), new IntPtr(ptr),
                                   0, null, null
                                   );
        }
    }
    return Tuple.Create(byteArray, shape);
}

private Tuple<sbyte[], long[]> createTuple_sbyte(CLMemoryHandle mem, CLCommandQueueHandle queue,
                                  int nbytes, long[] shape)
{
    var byteArray = new sbyte[1];
    if (nbytes > 0)
    {
        byteArray = new sbyte[nbytes / sizeof(sbyte)];
    }
    unsafe
    {
        fixed(void* ptr = &byteArray[0])
        {
            CL10.EnqueueReadBuffer(queue, mem, true,
                                   new IntPtr(0), new IntPtr(nbytes), new IntPtr(ptr),
                                   0, null, null
                                   );
        }
    }
    return Tuple.Create(byteArray, shape);
}

private Tuple<short[], long[]> createTuple_short(CLMemoryHandle mem, CLCommandQueueHandle queue,
                                  int nbytes, long[] shape)
{
    var byteArray = new short[1];
    if (nbytes > 0)
    {
        byteArray = new short[nbytes / sizeof(short)];
    }
    unsafe
    {
        fixed(void* ptr = &byteArray[0])
        {
            CL10.EnqueueReadBuffer(queue, mem, true,
                                   new IntPtr(0), new IntPtr(nbytes), new IntPtr(ptr),
                                   0, null, null
                                   );
        }
    }
    return Tuple.Create(byteArray, shape);
}

private Tuple<int[], long[]> createTuple_int(CLMemoryHandle mem, CLCommandQueueHandle queue,
                                  int nbytes, long[] shape)
{
    var byteArray = new int[1];
    if (nbytes > 0)
    {
        byteArray = new int[nbytes / sizeof(int)];
    }
    unsafe
    {
        fixed(void* ptr = &byteArray[0])
        {
            CL10.EnqueueReadBuffer(queue, mem, true,
                                   new IntPtr(0), new IntPtr(nbytes), new IntPtr(ptr),
                                   0, null, null
                                   );
        }
    }
    return Tuple.Create(byteArray, shape);
}

private Tuple<long[], long[]> createTuple_long(CLMemoryHandle mem, CLCommandQueueHandle queue,
                                int nbytes, long[] shape)
{
    var byteArray = new long[1];
    if (nbytes > 0)
    {
        byteArray = new long[nbytes / sizeof(long)];
    }
    unsafe
    {
        fixed(void* ptr = &byteArray[0])
        {
            CL10.EnqueueReadBuffer(queue, mem, true,
                                   new IntPtr(0), new IntPtr(nbytes), new IntPtr(ptr),
                                   0, null, null
                                   );
        }
    }
    return Tuple.Create(byteArray, shape);
}

private Tuple<float[], long[]> createTuple_float(CLMemoryHandle mem, CLCommandQueueHandle queue,
                                 int nbytes, long[] shape)
{
    var byteArray = new float[1];
    if (nbytes > 0)
    {
        byteArray = new float[nbytes / sizeof(float)];
    }
    unsafe
    {
        fixed(void* ptr = &byteArray[0])
        {
            CL10.EnqueueReadBuffer(queue, mem, true,
                                   new IntPtr(0), new IntPtr(nbytes), new IntPtr(ptr),
                                   0, null, null
                                   );
        }
    }
    return Tuple.Create(byteArray, shape);
}

private Tuple<double[], long[]> createTuple_double(CLMemoryHandle mem, CLCommandQueueHandle queue,
                                 int nbytes, long[] shape)
{
    var byteArray = new double[1];
    if (nbytes > 0)
    {
        byteArray = new double[nbytes / sizeof(double)];
    }
    unsafe
    {
        fixed(void* ptr = &byteArray[0])
        {
            CL10.EnqueueReadBuffer(queue, mem, true,
                                   new IntPtr(0), new IntPtr(nbytes), new IntPtr(ptr),
                                   0, null, null
                                   );
        }
    }
    return Tuple.Create(byteArray, shape);
}

private Tuple<bool[], long[]> createTuple_bool(CLMemoryHandle mem, CLCommandQueueHandle queue,
                                 int nbytes, long[] shape)
{
    var byteArray = new bool[1];
    if (nbytes > 0)
    {
        byteArray = new bool[nbytes / sizeof(bool)];
    }
    unsafe
    {
        fixed(void* ptr = &byteArray[0])
        {
            CL10.EnqueueReadBuffer(queue, mem, true,
                                   new IntPtr(0), new IntPtr(nbytes), new IntPtr(ptr),
                                   0, null, null
                                   );
        }
    }
    return Tuple.Create(byteArray, shape);
}

