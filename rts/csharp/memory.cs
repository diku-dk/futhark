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

byte[] allocateMem(sbyte size)
{
    return new byte[size];
}
byte[] allocateMem(short size)
{
    return new byte[size];
}
byte[] allocateMem(int size)
{
    return new byte[size];
}
byte[] allocateMem(long size)
{
    return new byte[size];
}
byte[] allocateMem(byte size)
{
    return new byte[size];
}
byte[] allocateMem(ushort size)
{
    return new byte[size];
}
byte[] allocateMem(uint size)
{
    return new byte[size];
}
byte[] allocateMem(ulong size)
{
    return new byte[size];
}

Tuple<byte[], long[]> createTuple_byte(CLMemoryHandle mem, CLCommandQueueHandle queue,
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

Tuple<byte[], long[]> createTuple_byte(byte[] bytes, long[] shape)
{
    var byteArray = new byte[bytes.Length / sizeof(byte)];
    Buffer.BlockCopy(bytes, 0, byteArray, 0, bytes.Length);
    return Tuple.Create(byteArray, shape);
}

Tuple<ushort[], long[]> createTuple_ushort(CLMemoryHandle mem, CLCommandQueueHandle queue,
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

Tuple<ushort[], long[]> createTuple_ushort(byte[] bytes, long[] shape)
{
    var ushortArray = new ushort[bytes.Length / sizeof(ushort)];
    Buffer.BlockCopy(bytes, 0, ushortArray, 0, bytes.Length);
    return Tuple.Create(ushortArray, shape);
}




Tuple<uint[], long[]> createTuple_uint(CLMemoryHandle mem, CLCommandQueueHandle queue,
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
Tuple<uint[], long[]> createTuple_uint(byte[] bytes, long[] shape)
{
    var uintArray = new uint[bytes.Length / sizeof(uint)];
    Buffer.BlockCopy(bytes, 0, uintArray, 0, bytes.Length);
    return Tuple.Create(uintArray, shape);
}

Tuple<ulong[], long[]> createTuple_ulong(CLMemoryHandle mem, CLCommandQueueHandle queue,
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

Tuple<ulong[], long[]> createTuple_ulong(byte[] bytes, long[] shape)
{
    var ulongArray = new ulong[bytes.Length / sizeof(ulong)];
    Buffer.BlockCopy(bytes, 0, ulongArray, 0, bytes.Length);
    return Tuple.Create(ulongArray, shape);
}

Tuple<sbyte[], long[]> createTuple_sbyte(CLMemoryHandle mem, CLCommandQueueHandle queue,
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

Tuple<sbyte[], long[]> createTuple_sbyte(byte[] bytes, long[] shape)
{
    var sbyteArray = new sbyte[1];
    if (bytes.Length > 0)
    {
        sbyteArray = new sbyte[bytes.Length / sizeof(sbyte)];
    }
    Buffer.BlockCopy(bytes, 0, sbyteArray, 0, bytes.Length);
    return Tuple.Create(sbyteArray, shape);
}

Tuple<short[], long[]> createTuple_short(CLMemoryHandle mem, CLCommandQueueHandle queue,
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

Tuple<short[], long[]> createTuple_short(byte[] bytes, long[] shape)
{
    var shortArray = new short[1];
    if (bytes.Length > 0)
    {
        shortArray = new short[bytes.Length / sizeof(short)];
    }
    Buffer.BlockCopy(bytes, 0, shortArray, 0, bytes.Length);
    return Tuple.Create(shortArray, shape);
}

Tuple<int[], long[]> createTuple_int(CLMemoryHandle mem, CLCommandQueueHandle queue,
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

Tuple<int[], long[]> createTuple_int(byte[] bytes, long[] shape)
{
    var intArray = new int[1];
    if (bytes.Length > 0)
    {
        intArray = new int[bytes.Length / sizeof(int)];
    }
    Buffer.BlockCopy(bytes, 0, intArray, 0, bytes.Length);
    return Tuple.Create(intArray, shape);
}

Tuple<long[], long[]> createTuple_long(CLMemoryHandle mem, CLCommandQueueHandle queue,
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
Tuple<long[], long[]> createTuple_long(byte[] bytes, long[] shape)
{
    var longArray = new long[1];
    if (bytes.Length > 0)
    {
        longArray = new long[bytes.Length / sizeof(long)];
    }
    Buffer.BlockCopy(bytes, 0, longArray, 0, bytes.Length);
    return Tuple.Create(longArray, shape);
}

Tuple<float[], long[]> createTuple_float(CLMemoryHandle mem, CLCommandQueueHandle queue,
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
Tuple<float[], long[]> createTuple_float(byte[] bytes, long[] shape)
{
    var floatArray = new float[1];
    if (bytes.Length > 0)
    {
        floatArray = new float[bytes.Length / sizeof(float)];
    }
    Buffer.BlockCopy(bytes, 0, floatArray, 0, bytes.Length);
    return Tuple.Create(floatArray, shape);
}

Tuple<double[], long[]> createTuple_double(CLMemoryHandle mem, CLCommandQueueHandle queue,
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

Tuple<double[], long[]> createTuple_double(byte[] bytes, long[] shape)
{
    var doubleArray = new double[1];
    if (bytes.Length > 0)
    {
        doubleArray = new double[bytes.Length / sizeof(double)];
    }
    Buffer.BlockCopy(bytes, 0, doubleArray, 0, bytes.Length);
    return Tuple.Create(doubleArray, shape);
}

Tuple<bool[], long[]> createTuple_bool(CLMemoryHandle mem, CLCommandQueueHandle queue,
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
Tuple<bool[], long[]> createTuple_bool(byte[] bytes, long[] shape)
{
    var boolArray = new bool[1];
    if (bytes.Length > 0)
    {
        boolArray = new bool[bytes.Length / sizeof(bool)];
    }
    Buffer.BlockCopy(bytes, 0, boolArray, 0, bytes.Length);
    return Tuple.Create(boolArray, shape);
}

/*
  # Helper functions dealing with memory blocks.
def allocateMem(size):
  return ct.cast((ct.c_byte * max(0,size))(), ct.POINTER(ct.c_byte))

# Copy an array if its is not-None.  This is important for treating
# Numpy arrays as flat memory, but has some overhead.
def normaliseArray(x):
  if (x.base is x) or (x.base is None):
    return x
  else:
    return x.copy()

def unwrapArray(x):
  return normaliseArray(x).ctypes.data_as(ct.POINTER(ct.c_byte))

def createArray(x, dim):
  return np.ctypeslib.as_array(x, shape=dim)

# An opaque Futhark value.
class opaque(object):
  def __init__(self, desc, *payload):
    self.data = payload
    self.desc = desc

  def __repr__(self):
    return "<opaque Futhark value of type {}>".format(self.desc)

*/



byte[] unwrapArray(Array src, int obj_size)
{
    var bytes = new byte[src.Length * obj_size];
    Buffer.BlockCopy(src, 0, bytes, 0, bytes.Length);
    return bytes;
}

T indexArray<T>(byte[] src, int offset, Func<byte[],int, T> converter)
{
    return converter(src, offset);
}

void writeScalarArray(byte[] dest, int offset, sbyte value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(sbyte*) dest_ptr = value;
        }
    }
}
void writeScalarArray(byte[] dest, int offset, byte value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(byte*) dest_ptr = value;
        }
    }
}
void writeScalarArray(byte[] dest, int offset, short value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(short*) dest_ptr = value;
        }
    }
}
void writeScalarArray(byte[] dest, int offset, ushort value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(ushort*) dest_ptr = value;
        }
    }
}
void writeScalarArray(byte[] dest, int offset, int value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(int*) dest_ptr = value;
        }
    }
}
void writeScalarArray(byte[] dest, int offset, uint value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(uint*) dest_ptr = value;
        }
    }
}
void writeScalarArray(byte[] dest, int offset, long value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(long*) dest_ptr = value;
        }
    }
}
void writeScalarArray(byte[] dest, int offset, ulong value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(ulong*) dest_ptr = value;
        }
    }
}
void writeScalarArray(byte[] dest, int offset, float value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(float*) dest_ptr = value;
        }
    }
}
void writeScalarArray(byte[] dest, int offset, double value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(double*) dest_ptr = value;
        }
    }
}
void writeScalarArray(byte[] dest, int offset, bool value)
{
    unsafe
    {
        fixed (byte* dest_ptr = &dest[offset])
        {
            *(bool*) dest_ptr = value;
        }
    }
}
