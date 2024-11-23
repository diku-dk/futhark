# Start of values.py.

# Hacky parser/reader/writer for values written in Futhark syntax.
# Used for reading stdin when compiling standalone programs with the
# Python code generator.

import numpy as np
import string
import struct
import sys


class ReaderInput:
    def __init__(self, f):
        self.f = f
        self.lookahead_buffer = []

    def get_char(self):
        if len(self.lookahead_buffer) == 0:
            return self.f.read(1)
        else:
            c = self.lookahead_buffer[0]
            self.lookahead_buffer = self.lookahead_buffer[1:]
            return c

    def unget_char(self, c):
        self.lookahead_buffer = [c] + self.lookahead_buffer

    def get_chars(self, n):
        n1 = min(n, len(self.lookahead_buffer))
        s = b"".join(self.lookahead_buffer[:n1])
        self.lookahead_buffer = self.lookahead_buffer[n1:]
        n2 = n - n1
        if n2 > 0:
            s += self.f.read(n2)
        return s

    def peek_char(self):
        c = self.get_char()
        if c:
            self.unget_char(c)
        return c


def skip_spaces(f):
    c = f.get_char()
    while c != None:
        if c.isspace():
            c = f.get_char()
        elif c == b"-":
            # May be line comment.
            if f.peek_char() == b"-":
                # Yes, line comment. Skip to end of line.
                while c != b"\n" and c != None:
                    c = f.get_char()
            else:
                break
        else:
            break
    if c:
        f.unget_char(c)


def parse_specific_char(f, expected):
    got = f.get_char()
    if got != expected:
        f.unget_char(got)
        raise ValueError
    return True


def parse_specific_string(f, s):
    # This funky mess is intended, and is caused by the fact that if `type(b) ==
    # bytes` then `type(b[0]) == int`, but we need to match each element with a
    # `bytes`, so therefore we make each character an array element
    b = s.encode("utf8")
    bs = [b[i : i + 1] for i in range(len(b))]
    read = []
    try:
        for c in bs:
            parse_specific_char(f, c)
            read.append(c)
        return True
    except ValueError:
        for c in read[::-1]:
            f.unget_char(c)
        raise


def optional(p, *args):
    try:
        return p(*args)
    except ValueError:
        return None


def optional_specific_string(f, s):
    c = f.peek_char()
    # This funky mess is intended, and is caused by the fact that if `type(b) ==
    # bytes` then `type(b[0]) == int`, but we need to match each element with a
    # `bytes`, so therefore we make each character an array element
    b = s.encode("utf8")
    bs = [b[i : i + 1] for i in range(len(b))]
    if c == bs[0]:
        return parse_specific_string(f, s)
    else:
        return False


def sepEndBy(p, sep, *args):
    elems = []
    x = optional(p, *args)
    if x != None:
        elems += [x]
        while optional(sep, *args) != None:
            x = optional(p, *args)
            if x == None:
                break
            else:
                elems += [x]
    return elems


# Assumes '0x' has already been read
def parse_hex_int(f):
    s = b""
    c = f.get_char()
    while c != None:
        if c in b"01234556789ABCDEFabcdef":
            s += c
            c = f.get_char()
        elif c == b"_":
            c = f.get_char()  # skip _
        else:
            f.unget_char(c)
            break
    return str(int(s, 16)).encode("utf8")  # ugh


def parse_int(f):
    s = b""
    c = f.get_char()
    if c == b"0" and f.peek_char() in b"xX":
        c = f.get_char()  # skip X
        return parse_hex_int(f)
    else:
        while c != None:
            if c.isdigit():
                s += c
                c = f.get_char()
            elif c == b"_":
                c = f.get_char()  # skip _
            else:
                f.unget_char(c)
                break
        if len(s) == 0:
            raise ValueError
        return s


def parse_int_signed(f):
    s = b""
    c = f.get_char()

    if c == b"-" and f.peek_char().isdigit():
        return c + parse_int(f)
    else:
        if c != b"+":
            f.unget_char(c)
        return parse_int(f)


def read_str_comma(f):
    skip_spaces(f)
    parse_specific_char(f, b",")
    return b","


def read_str_int(f, s):
    skip_spaces(f)
    x = int(parse_int_signed(f))
    optional_specific_string(f, s)
    return x


def read_str_uint(f, s):
    skip_spaces(f)
    x = int(parse_int(f))
    optional_specific_string(f, s)
    return x


def read_str_i8(f):
    return np.int8(read_str_int(f, "i8"))


def read_str_i16(f):
    return np.int16(read_str_int(f, "i16"))


def read_str_i32(f):
    return np.int32(read_str_int(f, "i32"))


def read_str_i64(f):
    return np.int64(read_str_int(f, "i64"))


def read_str_u8(f):
    return np.uint8(read_str_int(f, "u8"))


def read_str_u16(f):
    return np.uint16(read_str_int(f, "u16"))


def read_str_u32(f):
    return np.uint32(read_str_int(f, "u32"))


def read_str_u64(f):
    return np.uint64(read_str_int(f, "u64"))


def read_char(f):
    skip_spaces(f)
    parse_specific_char(f, b"'")
    c = f.get_char()
    parse_specific_char(f, b"'")
    return c


def read_str_hex_float(f, sign):
    int_part = parse_hex_int(f)
    parse_specific_char(f, b".")
    frac_part = parse_hex_int(f)
    parse_specific_char(f, b"p")
    exponent = parse_int(f)

    int_val = int(int_part, 16)
    frac_val = float(int(frac_part, 16)) / (16 ** len(frac_part))
    exp_val = int(exponent)

    total_val = (int_val + frac_val) * (2.0**exp_val)
    if sign == b"-":
        total_val = -1 * total_val

    return float(total_val)


def read_str_decimal(f):
    skip_spaces(f)
    c = f.get_char()
    if c == b"-":
        sign = b"-"
    else:
        f.unget_char(c)
        sign = b""

    # Check for hexadecimal float
    c = f.get_char()
    if c == "0" and (f.peek_char() in ["x", "X"]):
        f.get_char()
        return read_str_hex_float(f, sign)
    else:
        f.unget_char(c)

    bef = optional(parse_int, f)
    if bef == None:
        bef = b"0"
        parse_specific_char(f, b".")
        aft = parse_int(f)
    elif optional(parse_specific_char, f, b"."):
        aft = parse_int(f)
    else:
        aft = b"0"
    if optional(parse_specific_char, f, b"E") or optional(
        parse_specific_char, f, b"e"
    ):
        expt = parse_int_signed(f)
    else:
        expt = b"0"
    return float(sign + bef + b"." + aft + b"E" + expt)


def read_str_f16(f):
    skip_spaces(f)
    try:
        parse_specific_string(f, "f16.nan")
        return np.float32(np.nan)
    except ValueError:
        try:
            parse_specific_string(f, "f16.inf")
            return np.float32(np.inf)
        except ValueError:
            try:
                parse_specific_string(f, "-f16.inf")
                return np.float32(-np.inf)
            except ValueError:
                x = read_str_decimal(f)
                optional_specific_string(f, "f16")
                return x


def read_str_f32(f):
    skip_spaces(f)
    try:
        parse_specific_string(f, "f32.nan")
        return np.float32(np.nan)
    except ValueError:
        try:
            parse_specific_string(f, "f32.inf")
            return np.float32(np.inf)
        except ValueError:
            try:
                parse_specific_string(f, "-f32.inf")
                return np.float32(-np.inf)
            except ValueError:
                x = read_str_decimal(f)
                optional_specific_string(f, "f32")
                return x


def read_str_f64(f):
    skip_spaces(f)
    try:
        parse_specific_string(f, "f64.nan")
        return np.float64(np.nan)
    except ValueError:
        try:
            parse_specific_string(f, "f64.inf")
            return np.float64(np.inf)
        except ValueError:
            try:
                parse_specific_string(f, "-f64.inf")
                return np.float64(-np.inf)
            except ValueError:
                x = read_str_decimal(f)
                optional_specific_string(f, "f64")
                return x


def read_str_bool(f):
    skip_spaces(f)
    if f.peek_char() == b"t":
        parse_specific_string(f, "true")
        return True
    elif f.peek_char() == b"f":
        parse_specific_string(f, "false")
        return False
    else:
        raise ValueError


def read_str_empty_array(f, type_name, rank):
    parse_specific_string(f, "empty")
    parse_specific_char(f, b"(")
    dims = []
    for i in range(rank):
        parse_specific_string(f, "[")
        dims += [int(parse_int(f))]
        parse_specific_string(f, "]")
    if np.prod(dims) != 0:
        raise ValueError
    parse_specific_string(f, type_name)
    parse_specific_char(f, b")")

    return tuple(dims)


def read_str_array_elems(f, elem_reader, type_name, rank):
    skip_spaces(f)
    try:
        parse_specific_char(f, b"[")
    except ValueError:
        return read_str_empty_array(f, type_name, rank)
    else:
        xs = sepEndBy(elem_reader, read_str_comma, f)
        skip_spaces(f)
        parse_specific_char(f, b"]")
        return xs


def read_str_array_helper(f, elem_reader, type_name, rank):
    def nested_row_reader(_):
        return read_str_array_helper(f, elem_reader, type_name, rank - 1)

    if rank == 1:
        row_reader = elem_reader
    else:
        row_reader = nested_row_reader
    return read_str_array_elems(f, row_reader, type_name, rank)


def expected_array_dims(l, rank):
    if rank > 1:
        n = len(l)
        if n == 0:
            elem = []
        else:
            elem = l[0]
        return [n] + expected_array_dims(elem, rank - 1)
    else:
        return [len(l)]


def verify_array_dims(l, dims):
    if dims[0] != len(l):
        raise ValueError
    if len(dims) > 1:
        for x in l:
            verify_array_dims(x, dims[1:])


def read_str_array(f, elem_reader, type_name, rank, bt):
    elems = read_str_array_helper(f, elem_reader, type_name, rank)
    if type(elems) == tuple:
        # Empty array
        return np.empty(elems, dtype=bt)
    else:
        dims = expected_array_dims(elems, rank)
        verify_array_dims(elems, dims)
        return np.array(elems, dtype=bt)


################################################################################

READ_BINARY_VERSION = 2

# struct format specified at
# https://docs.python.org/2/library/struct.html#format-characters


def mk_bin_scalar_reader(t):
    def bin_reader(f):
        fmt = FUTHARK_PRIMTYPES[t]["bin_format"]
        size = FUTHARK_PRIMTYPES[t]["size"]
        tf = FUTHARK_PRIMTYPES[t]["numpy_type"]
        return tf(struct.unpack("<" + fmt, f.get_chars(size))[0])

    return bin_reader


read_bin_i8 = mk_bin_scalar_reader("i8")
read_bin_i16 = mk_bin_scalar_reader("i16")
read_bin_i32 = mk_bin_scalar_reader("i32")
read_bin_i64 = mk_bin_scalar_reader("i64")

read_bin_u8 = mk_bin_scalar_reader("u8")
read_bin_u16 = mk_bin_scalar_reader("u16")
read_bin_u32 = mk_bin_scalar_reader("u32")
read_bin_u64 = mk_bin_scalar_reader("u64")

read_bin_f16 = mk_bin_scalar_reader("f16")
read_bin_f32 = mk_bin_scalar_reader("f32")
read_bin_f64 = mk_bin_scalar_reader("f64")

read_bin_bool = mk_bin_scalar_reader("bool")


def read_is_binary(f):
    skip_spaces(f)
    c = f.get_char()
    if c == b"b":
        bin_version = read_bin_u8(f)
        if bin_version != READ_BINARY_VERSION:
            panic(
                1,
                "binary-input: File uses version %i, but I only understand version %i.\n",
                bin_version,
                READ_BINARY_VERSION,
            )
        return True
    else:
        f.unget_char(c)
        return False


FUTHARK_PRIMTYPES = {
    "i8": {
        "binname": b"  i8",
        "size": 1,
        "bin_reader": read_bin_i8,
        "str_reader": read_str_i8,
        "bin_format": "b",
        "numpy_type": np.int8,
    },
    "i16": {
        "binname": b" i16",
        "size": 2,
        "bin_reader": read_bin_i16,
        "str_reader": read_str_i16,
        "bin_format": "h",
        "numpy_type": np.int16,
    },
    "i32": {
        "binname": b" i32",
        "size": 4,
        "bin_reader": read_bin_i32,
        "str_reader": read_str_i32,
        "bin_format": "i",
        "numpy_type": np.int32,
    },
    "i64": {
        "binname": b" i64",
        "size": 8,
        "bin_reader": read_bin_i64,
        "str_reader": read_str_i64,
        "bin_format": "q",
        "numpy_type": np.int64,
    },
    "u8": {
        "binname": b"  u8",
        "size": 1,
        "bin_reader": read_bin_u8,
        "str_reader": read_str_u8,
        "bin_format": "B",
        "numpy_type": np.uint8,
    },
    "u16": {
        "binname": b" u16",
        "size": 2,
        "bin_reader": read_bin_u16,
        "str_reader": read_str_u16,
        "bin_format": "H",
        "numpy_type": np.uint16,
    },
    "u32": {
        "binname": b" u32",
        "size": 4,
        "bin_reader": read_bin_u32,
        "str_reader": read_str_u32,
        "bin_format": "I",
        "numpy_type": np.uint32,
    },
    "u64": {
        "binname": b" u64",
        "size": 8,
        "bin_reader": read_bin_u64,
        "str_reader": read_str_u64,
        "bin_format": "Q",
        "numpy_type": np.uint64,
    },
    "f16": {
        "binname": b" f16",
        "size": 2,
        "bin_reader": read_bin_f16,
        "str_reader": read_str_f16,
        "bin_format": "e",
        "numpy_type": np.float16,
    },
    "f32": {
        "binname": b" f32",
        "size": 4,
        "bin_reader": read_bin_f32,
        "str_reader": read_str_f32,
        "bin_format": "f",
        "numpy_type": np.float32,
    },
    "f64": {
        "binname": b" f64",
        "size": 8,
        "bin_reader": read_bin_f64,
        "str_reader": read_str_f64,
        "bin_format": "d",
        "numpy_type": np.float64,
    },
    "bool": {
        "binname": b"bool",
        "size": 1,
        "bin_reader": read_bin_bool,
        "str_reader": read_str_bool,
        "bin_format": "b",
        "numpy_type": bool,
    },
}


def read_bin_read_type(f):
    read_binname = f.get_chars(4)

    for k, v in FUTHARK_PRIMTYPES.items():
        if v["binname"] == read_binname:
            return k
    panic(1, "binary-input: Did not recognize the type '%s'.\n", read_binname)


def numpy_type_to_type_name(t):
    for k, v in FUTHARK_PRIMTYPES.items():
        if v["numpy_type"] == t:
            return k
    raise Exception("Unknown Numpy type: {}".format(t))


def read_bin_ensure_scalar(f, expected_type):
    dims = read_bin_i8(f)

    if dims != 0:
        panic(
            1,
            "binary-input: Expected scalar (0 dimensions), but got array with %i dimensions.\n",
            dims,
        )

    bin_type = read_bin_read_type(f)
    if bin_type != expected_type:
        panic(
            1,
            "binary-input: Expected scalar of type %s but got scalar of type %s.\n",
            expected_type,
            bin_type,
        )


# ------------------------------------------------------------------------------
# General interface for reading Primitive Futhark Values
# ------------------------------------------------------------------------------


def read_scalar(f, ty):
    if read_is_binary(f):
        read_bin_ensure_scalar(f, ty)
        return FUTHARK_PRIMTYPES[ty]["bin_reader"](f)
    return FUTHARK_PRIMTYPES[ty]["str_reader"](f)


def read_array(f, expected_type, rank):
    if not read_is_binary(f):
        str_reader = FUTHARK_PRIMTYPES[expected_type]["str_reader"]
        return read_str_array(
            f,
            str_reader,
            expected_type,
            rank,
            FUTHARK_PRIMTYPES[expected_type]["numpy_type"],
        )

    bin_rank = read_bin_u8(f)

    if bin_rank != rank:
        panic(
            1,
            "binary-input: Expected %i dimensions, but got array with %i dimensions.\n",
            rank,
            bin_rank,
        )

    bin_type_enum = read_bin_read_type(f)
    if expected_type != bin_type_enum:
        panic(
            1,
            "binary-input: Expected %iD-array with element type '%s' but got %iD-array with element type '%s'.\n",
            rank,
            expected_type,
            bin_rank,
            bin_type_enum,
        )

    shape = []
    elem_count = 1
    for i in range(rank):
        bin_size = read_bin_i64(f)
        elem_count *= bin_size
        shape.append(bin_size)

    bin_fmt = FUTHARK_PRIMTYPES[bin_type_enum]["bin_format"]

    # We first read the expected number of types into a bytestring,
    # then use np.frombuffer.  This is because np.fromfile does not
    # work on things that are insufficiently file-like, like a network
    # stream.
    bytes = f.get_chars(elem_count * FUTHARK_PRIMTYPES[expected_type]["size"])
    arr = np.frombuffer(
        bytes, dtype=FUTHARK_PRIMTYPES[bin_type_enum]["numpy_type"]
    )
    arr.shape = shape

    return arr.copy()  # To ensure it is writeable.


if sys.version_info >= (3, 0):
    input_reader = ReaderInput(sys.stdin.buffer)
else:
    input_reader = ReaderInput(sys.stdin)

import re


def read_value(type_desc, reader=input_reader):
    """Read a value of the given type.  The type is a string
    representation of the Futhark type."""
    m = re.match(r"((?:\[\])*)([a-z0-9]+)$", type_desc)
    if m:
        dims = int(len(m.group(1)) / 2)
        basetype = m.group(2)
    assert m and basetype in FUTHARK_PRIMTYPES, "Unknown type: {}".format(
        type_desc
    )
    if dims > 0:
        return read_array(reader, basetype, dims)
    else:
        return read_scalar(reader, basetype)


def end_of_input(entry, f=input_reader):
    skip_spaces(f)
    if f.get_char() != b"":
        panic(1, 'Expected EOF on stdin after reading input for "%s".', entry)


def write_value_text(v, out=sys.stdout):
    if type(v) == np.uint8:
        out.write("%uu8" % v)
    elif type(v) == np.uint16:
        out.write("%uu16" % v)
    elif type(v) == np.uint32:
        out.write("%uu32" % v)
    elif type(v) == np.uint64:
        out.write("%uu64" % v)
    elif type(v) == np.int8:
        out.write("%di8" % v)
    elif type(v) == np.int16:
        out.write("%di16" % v)
    elif type(v) == np.int32:
        out.write("%di32" % v)
    elif type(v) == np.int64:
        out.write("%di64" % v)
    elif type(v) in [bool, np.bool_]:
        if v:
            out.write("true")
        else:
            out.write("false")
    elif type(v) == np.float16:
        if np.isnan(v):
            out.write("f16.nan")
        elif np.isinf(v):
            if v >= 0:
                out.write("f16.inf")
            else:
                out.write("-f16.inf")
        else:
            out.write("%.6ff16" % v)
    elif type(v) == np.float32:
        if np.isnan(v):
            out.write("f32.nan")
        elif np.isinf(v):
            if v >= 0:
                out.write("f32.inf")
            else:
                out.write("-f32.inf")
        else:
            out.write("%.6ff32" % v)
    elif type(v) == np.float64:
        if np.isnan(v):
            out.write("f64.nan")
        elif np.isinf(v):
            if v >= 0:
                out.write("f64.inf")
            else:
                out.write("-f64.inf")
        else:
            out.write("%.6ff64" % v)
    elif type(v) == np.ndarray:
        if np.prod(v.shape) == 0:
            tname = numpy_type_to_type_name(v.dtype)
            out.write(
                "empty({}{})".format(
                    "".join(["[{}]".format(d) for d in v.shape]), tname
                )
            )
        else:
            first = True
            out.write("[")
            for x in v:
                if not first:
                    out.write(", ")
                first = False
                write_value(x, out=out)
            out.write("]")
    else:
        raise Exception("Cannot print value of type {}: {}".format(type(v), v))


type_strs = {
    np.dtype("int8"): b"  i8",
    np.dtype("int16"): b" i16",
    np.dtype("int32"): b" i32",
    np.dtype("int64"): b" i64",
    np.dtype("uint8"): b"  u8",
    np.dtype("uint16"): b" u16",
    np.dtype("uint32"): b" u32",
    np.dtype("uint64"): b" u64",
    np.dtype("float16"): b" f16",
    np.dtype("float32"): b" f32",
    np.dtype("float64"): b" f64",
    np.dtype("bool"): b"bool",
}


def construct_binary_value(v):
    t = v.dtype
    shape = v.shape

    elems = 1
    for d in shape:
        elems *= d

    num_bytes = 1 + 1 + 1 + 4 + len(shape) * 8 + elems * t.itemsize
    bytes = bytearray(num_bytes)
    bytes[0] = np.int8(ord("b"))
    bytes[1] = 2
    bytes[2] = np.int8(len(shape))
    bytes[3:7] = type_strs[t]

    for i in range(len(shape)):
        bytes[7 + i * 8 : 7 + (i + 1) * 8] = np.int64(shape[i]).tobytes()

    bytes[7 + len(shape) * 8 :] = np.ascontiguousarray(v).tobytes()

    return bytes


def write_value_binary(v, out=sys.stdout):
    if sys.version_info >= (3, 0):
        out = out.buffer
    out.write(construct_binary_value(v))


def write_value(v, out=sys.stdout, binary=False):
    if binary:
        return write_value_binary(v, out=out)
    else:
        return write_value_text(v, out=out)


# End of values.py.
