private Stream s;
private BinaryReader b;

// Note that the lookahead buffer does not interact well with
// binary reading.  We are careful to not let this become a
// problem.
private Stack<char> LookaheadBuffer = new Stack<char>();

private void ResetLookahead(){
    LookaheadBuffer.Clear();
}

private void ValueReader(Stream s)
{
    this.s = s;
}

private void ValueReader()
{
    this.s = Console.OpenStandardInput();
    this.b = new BinaryReader(s);
}

private char? GetChar()
{
    char c;
    if (LookaheadBuffer.Count == 0)
    {
        c = (char) this.b.ReadByte();
    }
    else
    {
        c = LookaheadBuffer.Pop();
    }

    return c;
}

private char[] GetChars(int n)
{
    return Enumerable.Range(0, n).Select(_ => GetChar().Value).ToArray();
}

private void UngetChar(char c)
{
    LookaheadBuffer.Push(c);
}

private char PeekChar()
{
    var c = GetChar();
    UngetChar(c.Value);
    return c.Value;
}

private void SkipSpaces()
{
    var c = GetChar();
    while (c.HasValue){
        if (char.IsWhiteSpace(c.Value))
        {
            c = GetChar();
        }
        else if (c == '-')
        {
            if (PeekChar() == '-')
            {
                while (c.Value != '\n')
                {
                    c = GetChar();
                }
            }
            else
            {
                break;
            }
        }
        else
        {
            break;
        }
    }

    if (c.HasValue)
    {
        UngetChar(c.Value);
    }
}

private bool ParseSpecificChar(char c)
{
    var got = GetChar();
    if (got.Value != c)
    {
        UngetChar(got.Value);
        throw new ValueError();
    }
    return true;
}

private bool ParseSpecificString(string str)
{
    var read = new List<char>();
    foreach (var c in str.ToCharArray())
    {
        try
        {
            ParseSpecificChar(c);
            read.Add(c);
        }
        catch(ValueError)
        {
            read.Reverse();
            foreach (var cc in read)
            {
                UngetChar(cc);
            }
            throw;
        }
    }

    return true;
}

private string Optional(Func<string> p)
{
    string res = null;
    try
    {
        res = p();
    }
    catch (Exception)
    {
    }

    return res;
}

private bool Optional(Func<char, bool> p, char c)
{
    try
    {
        return p(c);
    }
    catch (Exception)
    {
    }

    return false;
}

private bool OptionalSpecificString(string s)
{
    var c = PeekChar();
    if (c == s[0])
    {
        return ParseSpecificString(s);
    }
    return false;
}


private List<string> sepBy(Func<string> p, Func<string> sep)
{
    var elems = new List<string>();
    var x = Optional(p);
    if (!string.IsNullOrWhiteSpace(x))
    {
        elems.Add(x);
        while (!string.IsNullOrWhiteSpace(Optional(sep)))
        {
            var y = Optional(p);
            elems.Add(y);
        }
    }
    return elems;
}

private string ParseHexInt()
{
    var s = "";
    var c = GetChar();
    while (c.HasValue)
    {
        if (Uri.IsHexDigit(c.Value))
        {
            s += c.Value;
            c = GetChar();
        }
        else if (c == '_')
        {
            c = GetChar();
        }
        else
        {
            UngetChar(c.Value);
            break;
        }
    }

    return Convert.ToString(Convert.ToUInt32(s, 16));
}

private string ParseInt()
{
    var s = "";
    var c = GetChar();
    if (c.Value == '0' && "xX".Contains(PeekChar()))
    {
        GetChar();
        s += ParseHexInt();
    }
    else
    {
        while (c.HasValue)
        {
            if (char.IsDigit(c.Value))
            {
                s += c.Value;
                c = GetChar();
            }else if (c == '_')
            {
                c = GetChar();
            }
            else
            {
                UngetChar(c.Value);
                break;
            }
        }

    }

    if (s.Length == 0)
    {
        throw new Exception("ValueError");
    }

    return s;
}

private string ParseIntSigned()
{
    var c = GetChar();
    if (c.Value == '-' && char.IsDigit(PeekChar()))
    {
        return c + ParseInt();
    }
    else
    {
        if (c.Value != '+')
        {
            UngetChar(c.Value);
        }

        return ParseInt();
    }
}

private string ReadStrComma()
{
    SkipSpaces();
    ParseSpecificChar(',');
    return ",";
}

private int ReadStrInt(string s)
{
    SkipSpaces();
    var x = Convert.ToInt32(ParseIntSigned());
    OptionalSpecificString(s);
    return x;
}

private ulong ReadStrUInt64(string s)
{
    SkipSpaces();
    var x = Convert.ToUInt64(ParseInt());
    OptionalSpecificString(s);
    return x;
}

private long ReadStrInt64(string s)
{
    SkipSpaces();
    var x = Convert.ToInt64(ParseIntSigned());
    OptionalSpecificString(s);
    return x;
}

private uint ReadStrUInt(string s)
{
    SkipSpaces();
    var x = Convert.ToUInt32(ParseInt());
    OptionalSpecificString(s);
    return x;
}

private int ReadStrI8(){return ReadStrInt("i8");}
private int ReadStrI16(){return ReadStrInt("i16");}
private int ReadStrI32(){return ReadStrInt("i32");}
private long ReadStrI64(){return ReadStrInt64("i64");}
private uint ReadStrU8(){return ReadStrUInt("u8");}
private uint ReadStrU16(){return ReadStrUInt("u16");}
private uint ReadStrU32(){return ReadStrUInt("u32");}
private ulong ReadStrU64(){return ReadStrUInt64("u64");}
private sbyte ReadBinI8(){return (sbyte) b.ReadByte();}
private short ReadBinI16(){return b.ReadInt16();}
private int ReadBinI32(){return b.ReadInt32();}
private long ReadBinI64(){return b.ReadInt64();}
private byte ReadBinU8(){return (byte) b.ReadByte();}
private ushort ReadBinU16(){return b.ReadUInt16();}
private uint ReadBinU32(){return b.ReadUInt32();}
private ulong ReadBinU64(){return b.ReadUInt64();}
private float ReadBinF32(){return b.ReadSingle();}
private double ReadBinF64(){return b.ReadDouble();}
private bool ReadBinBool(){return b.ReadBoolean();}

private char ReadChar()
{
    SkipSpaces();
    ParseSpecificChar('\'');
    var c = GetChar();
    ParseSpecificChar('\'');
    return c.Value;
}

private double ReadStrHexFloat(char sign)
{
    var int_part = ParseHexInt();
    ParseSpecificChar('.');
    var frac_part = ParseHexInt();
    ParseSpecificChar('p');
    var exponent = ParseHexInt();

    var int_val = Convert.ToInt32(int_part, 16);
    var frac_val = Convert.ToSingle(Convert.ToInt32(frac_part, 16)) / Math.Pow(16, frac_part.Length);
    var exp_val = Convert.ToInt32(exponent);

    var total_val = (int_val + frac_val) * Math.Pow(2, exp_val);
    if (sign == '-')
    {
        total_val = -1 * total_val;
    }

    return Convert.ToDouble(total_val);
}

private double ReadStrDecimal()
{
    SkipSpaces();
    var c = GetChar();
    char sign;
    if (c.Value == '-')
    {
        sign = '-';
    }
    else
    {
        UngetChar(c.Value);
        sign = '+';
    }

    // Check for hexadecimal float
    c = GetChar();
    if (c.Value == '0' && "xX".Contains(PeekChar()))
    {
        GetChar();
        return ReadStrHexFloat(sign);
    }
    else
    {
        UngetChar(c.Value);
    }

    var bef = Optional(this.ParseInt);
    var aft = "";
    if (string.IsNullOrEmpty(bef))
    {
        bef = "0";
        ParseSpecificChar('.');
        aft = ParseInt();
    }else if (Optional(ParseSpecificChar, '.'))
    {
        aft = ParseInt();
    }
    else
    {
        aft = "0";
    }

    var expt = "";
    if (Optional(ParseSpecificChar, 'E') ||
        Optional(ParseSpecificChar, 'e'))
    {
        expt = ParseIntSigned();
    }
    else
    {
        expt = "0";
    }

    return Convert.ToDouble(sign + bef + "." + aft + "E" + expt);
}

private float ReadStrF32()
{
    try
    {
        ParseSpecificString("f32.nan");
        return Single.NaN;
    }
    catch (ValueError)
    {
        try
        {
            ParseSpecificString("-f32.inf");
            return Single.NegativeInfinity;
        }
        catch (ValueError)
        {
            try
            {
                ParseSpecificString("f32.inf");
                return Single.PositiveInfinity;
            }
            catch (ValueError)
            {
                var x = ReadStrDecimal();
                OptionalSpecificString("f32");
                return Convert.ToSingle(x);
            }
        }
    }
}

private double ReadStrF64()
{
    try
    {
        ParseSpecificString("f64.nan");
        return Double.NaN;
    }
    catch (ValueError)
    {
        try
        {
            ParseSpecificString("-f64.inf");
            return Double.NegativeInfinity;
        }
        catch (ValueError)
        {
            try
            {
                ParseSpecificString("f64.inf");
                return Double.PositiveInfinity;
            }
            catch (ValueError)
            {
                var x = ReadStrDecimal();
                OptionalSpecificString("f64");
                return x;
            }
        }
    }
}
private bool ReadStrBool()
{
    SkipSpaces();
    if (PeekChar() == 't')
    {
        ParseSpecificString("true");
        return true;
    }

    if (PeekChar() == 'f')
    {
        ParseSpecificString("false");
        return false;
    }

    throw new ValueError();
}

private (T[], int[]) ReadStrArrayElems<T>(int rank, Func<T> ReadStrScalar)
{
    bool first = true;
    bool[] knows_dimsize = new bool[rank];
    int cur_dim = rank-1;
    int[] elems_read_in_dim = new int[rank];
    int[] shape = new int[rank];

    int capacity = 100;
    T[] data = new T[capacity];
    int write_ptr = 0;

    while (true) {
        SkipSpaces();

        char c = (char) GetChar();
        if (c == ']') {
            if (knows_dimsize[cur_dim]) {
                if (shape[cur_dim] != elems_read_in_dim[cur_dim]) {
                    throw new Exception("Irregular array");
                }
            } else {
                knows_dimsize[cur_dim] = true;
                shape[cur_dim] = elems_read_in_dim[cur_dim];
            }
            if (cur_dim == 0) {
                break;
            } else {
                cur_dim--;
                elems_read_in_dim[cur_dim]++;
            }
        } else if (c == ',') {
            SkipSpaces();
            c = (char) GetChar();
            if (c == '[') {
                if (cur_dim == rank - 1) {
                    throw new Exception("Array has too many dimensions");
                }
                first = true;
                cur_dim++;
                elems_read_in_dim[cur_dim] = 0;
            } else if (cur_dim == rank - 1) {
                UngetChar(c);

                data[write_ptr++] = ReadStrScalar();
                if (write_ptr == capacity) {
                    capacity *= 2;
                    Array.Resize(ref data, capacity);
                }
                elems_read_in_dim[cur_dim]++;
            } else {
                throw new Exception("Unexpected comma when reading array");
            }
        } else if (first) {
            if (c == '[') {
                if (cur_dim == rank - 1) {
                    throw new Exception("Array has too many dimensions");
                }
                cur_dim++;
                elems_read_in_dim[cur_dim] = 0;
            } else {
                UngetChar(c);
                data[write_ptr++] = ReadStrScalar();
                if (write_ptr == capacity) {
                    capacity *= 2;
                    Array.Resize(ref data, capacity);
                }
                elems_read_in_dim[cur_dim]++;
                first = false;
            }
        } else {
            throw new Exception("Unexpected character in array");
        }
    }
    Array.Resize(ref data, write_ptr);
    return (data, shape);
}

private (T[], int[]) ReadStrArrayEmpty<T>(int rank, string typeName, Func<T> ReadStrScalar)
{
    ParseSpecificString("empty");
    ParseSpecificChar('(');
    int[] shape = new int[rank];
    for (int i = 1; i < rank; i++) {
        ParseSpecificString("[");
        shape[i] = ReadStrInt("");
        ParseSpecificString("]");
    }
    ParseSpecificString(typeName);
    ParseSpecificChar(')');

    return (new T[1], shape);
}

private (T[], int[]) ReadStrArray<T>(int rank, string typeName, Func<T> ReadStrScalar)
{
    long read_dims = 0;

    while (true) {
        SkipSpaces();
        var c = GetChar();
        if (c=='[') {
            read_dims++;
        } else {
            if (c != null) {
                UngetChar((char)c);
            }
            break;
        }
    }

    if (read_dims == 0) {
        return ReadStrArrayEmpty(rank, typeName, ReadStrScalar);
    }

    if (read_dims != rank) {
        throw new Exception("Wrong number of dimensions");
    }

    return ReadStrArrayElems(rank, ReadStrScalar);
}

private Dictionary<string, string> primtypes = new Dictionary<string, string>
{
    {"  i8",   "i8"},
    {" i16",  "i16"},
    {" i32",  "i32"},
    {" i64",  "i64"},
    {"  u8",   "u8"},
    {" u16",  "u16"},
    {" u32",  "u32"},
    {" u64",  "u64"},
    {" f32",  "f32"},
    {" f64",  "f64"},
    {"bool", "bool"}
};

private int BINARY_FORMAT_VERSION = 2;


private void read_le_2byte(ref short dest)
{
    dest = b.ReadInt16();
}

private void read_le_4byte(ref int dest)
{
    dest = b.ReadInt32();
}

private void read_le_8byte(ref long dest)
{
    dest = b.ReadInt64();
}

private bool ReadIsBinary()
    {
        SkipSpaces();
        var c = GetChar();
        if (c == 'b')
        {
            byte bin_version = new byte();
            try
            {
                bin_version = (byte) b.ReadByte();
            }
            catch
            {
                Console.WriteLine("binary-input: could not read version");
                Environment.Exit(1);
            }

            if (bin_version != BINARY_FORMAT_VERSION)
            {
                Console.WriteLine((
                    "binary-input: File uses version {0}, but I only understand version {1}.", bin_version,
                    BINARY_FORMAT_VERSION));
                Environment.Exit(1);
            }

            return true;
        }
        UngetChar((char) c);
        return false;
    }

private (T[], int[]) ReadArray<T>(int rank, string typeName, Func<T> ReadStrScalar)
{
    if (!ReadIsBinary())
    {
        return ReadStrArray<T>(rank, typeName, ReadStrScalar);
    }
    else
    {
        return ReadBinArray<T>(rank, typeName, ReadStrScalar);
    }
}
private T ReadScalar<T>(string typeName, Func<T> ReadStrScalar, Func<T> ReadBinScalar)
{
    if (!ReadIsBinary())
    {
        return ReadStrScalar();
    }
    else
    {
        ReadBinEnsureScalar(typeName);
        return ReadBinScalar();
    }
}

private void ReadBinEnsureScalar(string typeName)
{
    var bin_dims = b.ReadByte();
    if (bin_dims != 0)
    {
        Console.WriteLine("binary-input: Expected scalar (0 dimensions), but got array with {0} dimensions.", bin_dims);
        Environment.Exit(1);
    }

    var bin_type = ReadBinReadTypeString();
    if (bin_type != typeName)
    {
        Console.WriteLine("binary-input: Expected scalar of type {0} but got scalar of type {1}.", typeName,
                          bin_type);
        Environment.Exit(1);
    }
}

private string ReadBinReadTypeString()
{
    var str_bytes = b.ReadBytes(4);
    var str = System.Text.Encoding.UTF8.GetString(str_bytes, 0, 4);
    return primtypes[str];
}

private (T[], int[]) ReadBinArray<T>(int rank, string typeName, Func<T> ReadStrScalar)
{
    var bin_dims = new int();
    var shape = new int[rank];
    try
    {
        bin_dims = b.ReadByte();
    }
    catch
    {
        Console.WriteLine("binary-input: Couldn't get dims.");
        Environment.Exit(1);
    }

    if (bin_dims != rank)
    {
        Console.WriteLine("binary-input: Expected {0} dimensions, but got array with {1} dimensions", rank,
            bin_dims);
        Environment.Exit(1);

    }

    var bin_primtype = ReadBinReadTypeString();
    if (typeName != bin_primtype)
    {
        Console.WriteLine("binary-input: Expected {0}D-array with element type '{1}', but got {2}D-array with element type '{3}'.",
                          rank, typeName, bin_dims, bin_primtype);
        Environment.Exit(1);
    }

    int elem_count = 1;
    for (var i = 0; i < rank; i++)
    {
        long bin_shape = new long();
        try
        {
            read_le_8byte(ref bin_shape);
        }
        catch
        {
            Console.WriteLine("binary-input: Couldn't read size for dimension {0} of array.", i);
            Environment.Exit(1);
        }

        elem_count *= (int) bin_shape;
        shape[i] = (int) bin_shape;
    }

    // For whatever reason, Marshal.SizeOf<bool> is 4, so special-case that here.
    var elem_size = typeof(T) == typeof(bool) ? 1 : Marshal.SizeOf<T>();
    var num_bytes = elem_count * elem_size;
    var tmp = new byte[num_bytes];
    var data = new T[elem_count];

    var to_read = num_bytes;
    var have_read = 0;
    while (to_read > 0)
    {
        var bytes_read = b.Read(tmp, have_read, to_read);
        to_read -= bytes_read;
        have_read += bytes_read;
        if (bytes_read == 0) {
            Console.WriteLine("binary-input: EOF after {0} bytes (expected {1})", have_read, num_bytes);
            Environment.Exit(1);
        }
    }

    if (!BitConverter.IsLittleEndian && elem_size != 1)
    {
        for (int i = 0; i < elem_count; i ++)
        {
            Array.Reverse(tmp, i * elem_size, elem_size); 
        }
    }
    Buffer.BlockCopy(tmp,0,data,0,num_bytes);

    /* we should have a proper error message here */
    return (data, shape);
}


private sbyte ReadI8()
{
    return (sbyte) ReadStrI8();
}
private short ReadI16()
{
    return (short) ReadStrI16();
}
private int ReadI32()
{
    return ReadStrI32();
}
private long ReadI64()
{
    return ReadStrI64();
}

private byte ReadU8()
{
    return (byte) ReadStrU8();
}
private ushort ReadU16()
{
    return (ushort) ReadStrU16();
}
private uint ReadU32()
{
    return (uint) ReadStrU32();
}
private ulong ReadU64()
{
    return (ulong) ReadStrU64();
}
private bool ReadBool()
{
    return ReadStrBool();
}
private float ReadF32()
{
    return ReadStrF32();
}
private double ReadF64()
{
    return ReadStrF64();
}

private void WriteValue(bool x){Console.Write(x ? "true" : "false", x);}
private void WriteValue(sbyte x){Console.Write("{0}i8", x);}
private void WriteValue(short x){Console.Write("{0}i16", x);}
private void WriteValue(int x){Console.Write("{0}i32", x);}
private void WriteValue(long x){Console.Write("{0}i64", x);}
private void WriteValue(byte x){Console.Write("{0}u8", x);}
private void WriteValue(ushort x){Console.Write("{0}u16", x);}
private void WriteValue(uint x){Console.Write("{0}u32", x);}
private void WriteValue(ulong x){Console.Write("{0}u64", x);}
private void WriteValue(float x){if (Single.IsNaN(x))
    {Console.Write("f32.nan");} else if (Single.IsNegativeInfinity(x))
    {Console.Write("-f32.inf");} else if (Single.IsPositiveInfinity(x))
    {Console.Write("f32.inf");} else
    {Console.Write("{0:0.000000}f32", x);}}
private void WriteValue(double x){if (Double.IsNaN(x))
    {Console.Write("f64.nan");} else if (Double.IsNegativeInfinity(x))
    {Console.Write("-f64.inf");} else if (Double.IsPositiveInfinity(x))
    {Console.Write("f64.inf");} else
    {Console.Write("{0:0.000000}f64", x);}}
