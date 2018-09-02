public struct FlatArray<T>
{
    public long[] shape;
    public T[] array;

    public FlatArray(T[] data_array, long[] shape_array)
    {
        shape = shape_array;
        array = data_array;
    }

    public FlatArray(T[] data_array)
    {
        shape = new long[] {data_array.Length};
        array = data_array;
    }

    private long getIdx(int[] idxs)
    {
        long idx = 0;
        for (int i = 0; i<idxs.Length; i++)
        {
            idx += shape[i] * idxs[i];
        }
        return idx;

    }
    public T this[params int[] indexes]
    {
        get
        {
            Debug.Assert(indexes.Length == shape.Length);
            return array[getIdx(indexes)];
        }

        set
        {
            Debug.Assert(indexes.Length == shape.Length);
            array[getIdx(indexes)] = value;
        }
    }

    public IEnumerator GetEnumerator()
    {
        foreach (T val in array)
        {
            yield return val;
        }
    }

    public (T[], long[]) AsTuple()
    {
        return (this.array, this.shape);
    }
}

public class Opaque{
    object desc;
    object data;
    public Opaque(string str, object payload)
    {
        this.desc = str;
        this.data = payload;
    }

    public override string ToString()
    {
        return string.Format("<opaque Futhark value of type {}>", desc);
    }
}

private byte[] allocateMem(sbyte size)
{
    return new byte[size];
}

private byte[] allocateMem(short size)
{
    return new byte[size];
}

private byte[] allocateMem(int size)
{
    return new byte[size];
}

private byte[] allocateMem(long size)
{
    return new byte[size];
}

private byte[] allocateMem(byte size)
{
    return new byte[size];
}

private byte[] allocateMem(ushort size)
{
    return new byte[size];
}

private byte[] allocateMem(uint size)
{
    return new byte[size];
}

private byte[] allocateMem(ulong size)
{
    return new byte[size];
}

private Tuple<byte[], long[]> createTuple_byte(byte[] bytes, long[] shape)
{
    var byteArray = new byte[bytes.Length / sizeof(byte)];
    Buffer.BlockCopy(bytes, 0, byteArray, 0, bytes.Length);
    return Tuple.Create(byteArray, shape);
}

private Tuple<ushort[], long[]> createTuple_ushort(byte[] bytes, long[] shape)
{
    var ushortArray = new ushort[bytes.Length / sizeof(ushort)];
    Buffer.BlockCopy(bytes, 0, ushortArray, 0, bytes.Length);
    return Tuple.Create(ushortArray, shape);
}

private Tuple<uint[], long[]> createTuple_uint(byte[] bytes, long[] shape)
{
    var uintArray = new uint[bytes.Length / sizeof(uint)];
    Buffer.BlockCopy(bytes, 0, uintArray, 0, bytes.Length);
    return Tuple.Create(uintArray, shape);
}

private Tuple<ulong[], long[]> createTuple_ulong(byte[] bytes, long[] shape)
{
    var ulongArray = new ulong[bytes.Length / sizeof(ulong)];
    Buffer.BlockCopy(bytes, 0, ulongArray, 0, bytes.Length);
    return Tuple.Create(ulongArray, shape);
}


private Tuple<sbyte[], long[]> createTuple_sbyte(byte[] bytes, long[] shape)
{
    var sbyteArray = new sbyte[1];
    if (bytes.Length > 0)
    {
        sbyteArray = new sbyte[bytes.Length / sizeof(sbyte)];
    }
    Buffer.BlockCopy(bytes, 0, sbyteArray, 0, bytes.Length);
    return Tuple.Create(sbyteArray, shape);
}


private Tuple<short[], long[]> createTuple_short(byte[] bytes, long[] shape)
{
    var shortArray = new short[1];
    if (bytes.Length > 0)
    {
        shortArray = new short[bytes.Length / sizeof(short)];
    }
    Buffer.BlockCopy(bytes, 0, shortArray, 0, bytes.Length);
    return Tuple.Create(shortArray, shape);
}

private Tuple<int[], long[]> createTuple_int(byte[] bytes, long[] shape)
{
    var intArray = new int[1];
    if (bytes.Length > 0)
    {
        intArray = new int[bytes.Length / sizeof(int)];
    }
    Buffer.BlockCopy(bytes, 0, intArray, 0, bytes.Length);
    return Tuple.Create(intArray, shape);
}

private Tuple<long[], long[]> createTuple_long(byte[] bytes, long[] shape)
{
    var longArray = new long[1];
    if (bytes.Length > 0)
    {
        longArray = new long[bytes.Length / sizeof(long)];
    }
    Buffer.BlockCopy(bytes, 0, longArray, 0, bytes.Length);
    return Tuple.Create(longArray, shape);
}

private Tuple<float[], long[]> createTuple_float(byte[] bytes, long[] shape)
{
    var floatArray = new float[1];
    if (bytes.Length > 0)
    {
        floatArray = new float[bytes.Length / sizeof(float)];
    }
    Buffer.BlockCopy(bytes, 0, floatArray, 0, bytes.Length);
    return Tuple.Create(floatArray, shape);
}


private Tuple<double[], long[]> createTuple_double(byte[] bytes, long[] shape)
{
    var doubleArray = new double[1];
    if (bytes.Length > 0)
    {
        doubleArray = new double[bytes.Length / sizeof(double)];
    }
    Buffer.BlockCopy(bytes, 0, doubleArray, 0, bytes.Length);
    return Tuple.Create(doubleArray, shape);
}

private Tuple<bool[], long[]> createTuple_bool(byte[] bytes, long[] shape)
{
    var boolArray = new bool[1];
    if (bytes.Length > 0)
    {
        boolArray = new bool[bytes.Length / sizeof(bool)];
    }
    Buffer.BlockCopy(bytes, 0, boolArray, 0, bytes.Length);
    return Tuple.Create(boolArray, shape);
}

private byte[] unwrapArray(Array src, int obj_size)
{
    var bytes = new byte[src.Length * obj_size];
    Buffer.BlockCopy(src, 0, bytes, 0, bytes.Length);
    return bytes;
}

private byte indexArray_byte(byte[] src, int offset)
{
    unsafe
    {
        fixed (void* dest_ptr = &src[offset])
        {
            return *(byte*) dest_ptr;
        }
    }
}

private ushort indexArray_ushort(byte[] src, int offset)
{
    unsafe
    {
        fixed (void* dest_ptr = &src[offset])
        {
            return *(ushort*) dest_ptr;
        }
    }
}

private uint indexArray_uint(byte[] src, int offset)
{
    unsafe
    {
        fixed (void* dest_ptr = &src[offset])
        {
            return *(uint*) dest_ptr;
        }
    }
}

private ulong indexArray_ulong(byte[] src, int offset)
{
    unsafe
    {
        fixed (void* dest_ptr = &src[offset])
        {
            return *(ulong*) dest_ptr;
        }
    }
}

private sbyte indexArray_sbyte(byte[] src, int offset)
{
    unsafe
    {
        fixed (void* dest_ptr = &src[offset])
        {
            return *(sbyte*) dest_ptr;
        }
    }
}

private short indexArray_short(byte[] src, int offset)
{
    unsafe
    {
        fixed (void* dest_ptr = &src[offset])
        {
            return *(short*) dest_ptr;
        }
    }
}

private int indexArray_int(byte[] src, int offset)
{
    unsafe
    {
        fixed (void* dest_ptr = &src[offset])
        {
            return *(int*) dest_ptr;
        }
    }
}

private long indexArray_long(byte[] src, int offset)
{
    unsafe
    {
        fixed (void* dest_ptr = &src[offset])
        {
            return *(long*) dest_ptr;
        }
    }
}

private float indexArray_float(byte[] src, int offset)
{
    unsafe
    {
        fixed (void* dest_ptr = &src[offset])
        {
            return *(float*) dest_ptr;
        }
    }
}

private double indexArray_double(byte[] src, int offset)
{
    unsafe
    {
        fixed (void* dest_ptr = &src[offset])
        {
            return *(double*) dest_ptr;
        }
    }
}

private bool indexArray_bool(byte[] src, int offset)
{
    unsafe
    {
        fixed (void* dest_ptr = &src[offset])
        {
            return *(bool*) dest_ptr;
        }
    }
}

private void writeScalarArray(byte[] dest, int offset, sbyte value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(sbyte*) dest_ptr = value;
        }
    }
}
private void writeScalarArray(byte[] dest, int offset, byte value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(byte*) dest_ptr = value;
        }
    }
}
private void writeScalarArray(byte[] dest, int offset, short value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(short*) dest_ptr = value;
        }
    }
}
private void writeScalarArray(byte[] dest, int offset, ushort value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(ushort*) dest_ptr = value;
        }
    }
}
private void writeScalarArray(byte[] dest, int offset, int value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(int*) dest_ptr = value;
        }
    }
}
private void writeScalarArray(byte[] dest, int offset, uint value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(uint*) dest_ptr = value;
        }
    }
}
private void writeScalarArray(byte[] dest, int offset, long value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(long*) dest_ptr = value;
        }
    }
}
private void writeScalarArray(byte[] dest, int offset, ulong value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(ulong*) dest_ptr = value;
        }
    }
}
private void writeScalarArray(byte[] dest, int offset, float value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(float*) dest_ptr = value;
        }
    }
}
private void writeScalarArray(byte[] dest, int offset, double value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(double*) dest_ptr = value;
        }
    }
}
private void writeScalarArray(byte[] dest, int offset, bool value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(bool*) dest_ptr = value;
        }
    }
}
