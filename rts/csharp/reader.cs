    private Stream s;
    private BinaryReader b;

    // Note that the lookahead buffer does not interact well with
    // binary reading.  We are careful to not let this become a
    // problem.
    private Stack<char> LookaheadBuffer = new Stack<char>();

    void ResetLookahead(){
        LookaheadBuffer.Clear();
    }

    public void ValueReader(Stream s)
    {
        this.s = s;
    }

    public void ValueReader()
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

    char[] GetChars(int n)
    {
        return Enumerable.Range(0, n).Select(_ => GetChar().Value).ToArray();
    }

    void UngetChar(char c)
    {
        LookaheadBuffer.Push(c);
    }

    char PeekChar()
    {
        var c = GetChar();
        UngetChar(c.Value);
        return c.Value;
    }

    void SkipSpaces()
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

    bool ParseSpecificChar(char c)
    {
        var got = GetChar();
        if (got.Value != c)
        {
            UngetChar(got.Value);
            throw new ValueError();
        }
        return true;
    }

    bool ParseSpecificString(string str)
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

    string Optional(Func<string> p)
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

    bool Optional(Func<char, bool> p, char c)
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

    bool OptionalSpecificString(string s)
    {
        var c = PeekChar();
        if (c == s[0])
        {
            return ParseSpecificString(s);
        }
        return false;
    }


    List<string> sepBy(Func<string> p, Func<string> sep)
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

    string ParseHexInt()
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

    string ParseInt()
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

    string ParseIntSigned()
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

    string ReadStrComma()
    {
        SkipSpaces();
        ParseSpecificChar(',');
        return ",";
    }

    int ReadStrInt(string s)
    {
        SkipSpaces();
        var x = Convert.ToInt32(ParseIntSigned());
        OptionalSpecificString(s);
        return x;
    }

    ulong ReadStrUInt64(string s)
    {
        SkipSpaces();
        var x = Convert.ToUInt64(ParseInt());
        OptionalSpecificString(s);
        return x;
    }

    long ReadStrInt64(string s)
    {
        SkipSpaces();
        var x = Convert.ToInt64(ParseIntSigned());
        OptionalSpecificString(s);
        return x;
    }

    uint ReadStrUInt(string s)
    {
        SkipSpaces();
        var x = Convert.ToUInt32(ParseInt());
        OptionalSpecificString(s);
        return x;
    }

    int ReadStrI8(){return ReadStrInt("i8");}
    int ReadStrI16(){return ReadStrInt("i16");}
    int ReadStrI32(){return ReadStrInt("i32");}
    long ReadStrI64(){return ReadStrInt64("i64");}
    uint ReadStrU8(){return ReadStrUInt("u8");}
    uint ReadStrU16(){return ReadStrUInt("u16");}
    uint ReadStrU32(){return ReadStrUInt("u32");}
    ulong ReadStrU64(){return ReadStrUInt64("u64");}
    sbyte ReadBinI8(){return (sbyte) b.ReadByte();}
    short ReadBinI16(){return b.ReadInt16();}
    int ReadBinI32(){return b.ReadInt32();}
    long ReadBinI64(){return b.ReadInt64();}
    byte ReadBinU8(){return (byte) b.ReadByte();}
    ushort ReadBinU16(){return b.ReadUInt16();}
    uint ReadBinU32(){return b.ReadUInt32();}
    ulong ReadBinU64(){return b.ReadUInt64();}
    float ReadBinF32(){return b.ReadSingle();}
    double ReadBinF64(){return b.ReadDouble();}
    bool ReadBinBool(){return b.ReadBoolean();}

    char ReadChar()
    {
        SkipSpaces();
        ParseSpecificChar('\'');
        var c = GetChar();
        ParseSpecificChar('\'');
        return c.Value;
    }

    double ReadStrHexFloat(char sign)
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

    double ReadStrDecimal()
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

    float ReadStrF32()
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

    double ReadStrF64()
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
    bool ReadStrBool()
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

    public (T[], int[]) ReadStrArrayEmpty<T>(int rank, string typeName, Func<T> ReadStrScalar)
    {
        ParseSpecificString("empty");
        ParseSpecificChar('(');
        for (int i = 0; i < rank-1; i++) {
            ParseSpecificString("[]");
        }
        ParseSpecificString(typeName);
        ParseSpecificChar(')');

        return (new T[1], new int[rank]);
    }

    public (T[], int[]) ReadStrArray<T>(int rank, string typeName, Func<T> ReadStrScalar)
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

    Dictionary<string, string> primtypes = new Dictionary<string, string>
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


    public void read_le_2byte(ref short dest)
    {
        dest = b.ReadInt16();
    }
    
    public void read_le_4byte(ref int dest)
    {
        dest = b.ReadInt32();
    }
    
    public void read_le_8byte(ref long dest)
    {
        dest = b.ReadInt64();
    }

    public bool ReadIsBinary()
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

    public (T[], int[]) ReadArray<T>(int rank, string typeName, Func<T> ReadStrScalar)
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
    public T ReadScalar<T>(string typeName, Func<T> ReadStrScalar, Func<T> ReadBinScalar)
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

    void ReadBinEnsureScalar(string typeName)
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
    public string ReadBinReadTypeString()
    {
        var str_bytes = b.ReadBytes(4);
        var str = System.Text.Encoding.UTF8.GetString(str_bytes, 0, 4);
        return primtypes[str];
    }

    public (T[], int[]) ReadBinArray<T>(int rank, string typeName, Func<T> ReadStrScalar)
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

        var elem_size = Marshal.SizeOf(typeof(T));
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


    public sbyte read_i8()
    {
        return (sbyte) ReadStrI8();
    }
    public short read_i16()
    {
        return (short) ReadStrI16();
    }
    public int read_i32()
    {
        return ReadStrI32();
    }
    public long read_i64()
    {
        return ReadStrI64();
    }

    public byte read_u8()
    {
        return (byte) ReadStrU8();
    }
    public ushort read_u16()
    {
        return (ushort) ReadStrU16();
    }
    public uint read_u32()
    {
        return (uint) ReadStrU32();
    }
    public ulong read_u64()
    {
        return (ulong) ReadStrU64();
    }

    public bool read_bool()
    {
        return ReadStrBool();
    }

    public float read_f32()
    {
        return ReadStrF32();
    }
    public double read_f64()
    {
        return ReadStrF64();
    }
    public sbyte read_bin_i8()
    {
        return (sbyte) ReadBinI8();
    }
    public short read_bin_i16()
    {
        return (short) ReadBinI16();
    }
    public int read_bin_i32()
    {
        return ReadBinI32();
    }
    public long read_bin_i64()
    {
        return ReadBinI64();
    }
    public byte read_bin_u8()
    {
        return (byte) ReadBinU8();
    }
    public ushort read_bin_u16()
    {
        return (ushort) ReadBinU16();
    }
    public uint read_bin_u32()
    {
        return (uint) ReadBinU32();
    }
    public ulong read_bin_u64()
    {
        return (ulong) ReadBinU64();
    }
    public bool read_bin_bool()
    {
        return ReadBinBool();
    }
    public float read_bin_f32()
    {
        return ReadBinF32();
    }
    public double read_bin_f64()
    {
        return ReadBinF64();
    }
    void WriteValue(string s, sbyte x){Console.Write(s, x);}
    void WriteValue(string s, short x){Console.Write(s, x);}
    void WriteValue(string s, int x){Console.Write(s, x);}
    void WriteValue(string s, long x){Console.Write(s, x);}
    void WriteValue(string s, byte x){Console.Write(s, x);}
    void WriteValue(string s, ushort x){Console.Write(s, x);}
    void WriteValue(string s, uint x){Console.Write(s, x);}
    void WriteValue(string s, ulong x){Console.Write(s, x);}
    void WriteValue(string s, float x){if (Single.IsNaN(x))
        {Console.Write("f32.nan");} else if (Single.IsNegativeInfinity(x))
        {Console.Write("-f32.inf");} else if (Single.IsPositiveInfinity(x))
        {Console.Write("f32.inf");} else
        {Console.Write(s, x);}}
    void WriteValue(string s, double x){if (Double.IsNaN(x))
        {Console.Write("f64.nan");} else if (Double.IsNegativeInfinity(x))
        {Console.Write("-f64.inf");} else if (Double.IsPositiveInfinity(x))
        {Console.Write("f64.inf");} else
        {Console.Write(s, x);}}
