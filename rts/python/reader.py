# Hacky parser/reader for values written in Futhark syntax.  Used for
# reading stdin when compiling standalone programs with the Python
# code generator.

import numpy as np
import string
import struct

lookahead_buffer = []

def reset_lookahead():
    global lookahead_buffer
    lookahead_buffer = []

def get_char(f):
    global lookahead_buffer
    if len(lookahead_buffer) == 0:
        return f.read(1)
    else:
        c = lookahead_buffer[0]
        lookahead_buffer = lookahead_buffer[1:]
        return c

def get_chars(f, n):
    s = ''
    for _ in range(n):
        s += get_char(f)
    return s

def unget_char(f, c):
    global lookahead_buffer
    lookahead_buffer = [c] + lookahead_buffer

def peek_char(f):
    c = get_char(f)
    if c:
        unget_char(f, c)
    return c

def skip_spaces(f):
    c = get_char(f)
    while c != None:
        if c.isspace():
            c = get_char(f)
        elif c == '-':
          # May be line comment.
          if peek_char(f) == '-':
            # Yes, line comment. Skip to end of line.
            while (c != '\n' and c != None):
              c = get_char(f)
          else:
            break
        else:
          break
    if c:
        unget_char(f, c)

def parse_specific_char(f, expected):
    got = get_char(f)
    if got != expected:
        unget_char(f, got)
        raise ValueError
    return True

def parse_specific_string(f, s):
    for c in s:
        parse_specific_char(f, c)
    return True

def optional(p, *args):
    try:
        return p(*args)
    except ValueError:
        return None

def optional_specific_string(f, s):
    c = peek_char(f)
    if c == s[0]:
        return parse_specific_string(f, s)
    else:
        return False

def sepBy(p, sep, *args):
    elems = []
    x = optional(p, *args)
    if x != None:
        elems += [x]
        while optional(sep, *args) != None:
            x = p(*args)
            elems += [x]
    return elems

def parse_int(f):
    s = ''
    c = get_char(f)
    if c == '0' and peek_char(f) in ['x', 'X']:
        c = get_char(f) # skip X
        c = get_char(f)
        while c != None:
            if c in string.hexdigits:
                s += c
                c = get_char(f)
            else:
                unget_char(f, c)
                s = str(int(s, 16))
                break
    else:
        while c != None:
            if c.isdigit():
                s += c
                c = get_char(f)
            else:
                unget_char(f, c)
                break
    if len(s) == 0:
        raise ValueError
    return s

def parse_int_signed(f):
    s = ''
    c = get_char(f)

    if c == '-' and peek_char(f).isdigit():
      s = c + parse_int(f)
    else:
      if c != '+':
          unget_char(f, c)
      s = parse_int(f)

    return s

def read_str_comma(f):
    skip_spaces(f)
    parse_specific_char(f, ',')
    return ','

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
    return read_str_int(f, 'i8')
def read_str_i16(f):
    return read_str_int(f, 'i16')
def read_str_i32(f):
    return read_str_int(f, 'i32')
def read_str_i64(f):
    return read_str_int(f, 'i64')

def read_str_u8(f):
    return read_str_int(f, 'i8')
def read_str_u16(f):
    return read_str_int(f, 'i16')
def read_str_u32(f):
    return read_str_int(f, 'i32')
def read_str_u64(f):
    return read_str_int(f, 'i64')

def read_char(f):
    skip_spaces(f)
    parse_specific_char(f, '\'')
    c = get_char(f)
    parse_specific_char(f, '\'')
    return c

def read_str_decimal(f):
    skip_spaces(f)
    c = get_char(f)
    if (c == '-'):
      sign = '-'
    else:
      unget_char(f,c)
      sign = ''
    bef = optional(parse_int, f)
    if bef == None:
        bef = '0'
        parse_specific_char(f, '.')
        aft = parse_int(f)
    elif optional(parse_specific_char, f, '.'):
        aft = parse_int(f)
    else:
        aft = '0'
    if (optional(parse_specific_char, f, 'E') or
        optional(parse_specific_char, f, 'e')):
        expt = parse_int_signed(f)
    else:
        expt = '0'
    return float(sign + bef + '.' + aft + 'E' + expt)

def read_str_f32(f):
    x = read_str_decimal(f)
    optional_specific_string(f, 'f32')
    return x

def read_str_f64(f):
    x = read_str_decimal(f)
    optional_specific_string(f, 'f64')
    return x

def read_str_bool(f):
    skip_spaces(f)
    if peek_char(f) == 't':
        parse_specific_string(f, 'true')
        return True
    elif peek_char(f) == 'f':
        parse_specific_string(f, 'false')
        return False
    else:
        raise ValueError

def read_str_empty_array(f, type_name, rank):
    parse_specific_string(f, 'empty')
    parse_specific_char(f, '(')
    for i in range(rank):
        parse_specific_string(f, '[]')
    parse_specific_string(f, type_name)
    parse_specific_char(f, ')')
    return []

def read_str_array_elems(f, elem_reader, type_name, rank):
    skip_spaces(f)
    try:
        parse_specific_char(f, '[')
    except ValueError:
        return read_str_empty_array(f, type_name, rank)
    else:
        xs = sepBy(elem_reader, read_str_comma, f)
        skip_spaces(f)
        parse_specific_char(f, ']')
        return xs

def read_str_array_helper(f, elem_reader, type_name, rank):
    def nested_row_reader(_):
        return read_str_array_helper(f, elem_reader, type_name, rank-1)
    if rank == 1:
        row_reader = elem_reader
    else:
        row_reader = nested_row_reader
    return read_str_array_elems(f, row_reader, type_name, rank-1)

def expected_array_dims(l, rank):
  if rank > 1:
      n = len(l)
      if n == 0:
          elem = []
      else:
          elem = l[0]
      return [n] + expected_array_dims(elem, rank-1)
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
    dims = expected_array_dims(elems, rank)
    verify_array_dims(elems, dims)
    return np.array(elems, dtype=bt)

################################################################################

READ_BINARY_VERSION = 1

# Format specified at https://docs.python.org/2/library/struct.html#format-characters

def read_bin_i8(f):
    s = get_chars(f, 1)
    return struct.unpack('b', s)[0]

def read_bin_i16(f):
    s = get_chars(f, 2)
    return struct.unpack('<h', s)[0]

def read_bin_i32(f):
    s = get_chars(f, 4)
    return struct.unpack('<i', s)[0]

def read_bin_i64(f):
    s = get_chars(f, 8)
    return struct.unpack('<q', s)[0]

def read_bin_u8(f):
    s = get_chars(f, 1)
    return struct.unpack('B', s)[0]

def read_bin_u16(f):
    s = get_chars(f, 2)
    return struct.unpack('<H', s)[0]

def read_bin_u32(f):
    s = get_chars(f, 4)
    return struct.unpack('<I', s)[0]

def read_bin_u64(f):
    s = get_chars(f, 8)
    return struct.unpack('<Q', s)[0]

def read_bin_f32(f):
    s = get_chars(f, 4)
    return struct.unpack('<f', s)[0]

def read_bin_f64(f):
    s = get_chars(f, 8)
    return struct.unpack('<d', s)[0]

def read_bin_bool(f):
    return read_bin_i8(f) != 0

def read_is_binary(f):
    skip_spaces(f)
    c = get_char(f)
    if c == 'b':
        bin_version = read_bin_u8(f)
        if bin_version != READ_BINARY_VERSION:
            panic(1, "binary-input: File uses version %i, but I only understand version %i.\n",
                  bin_version, READ_BINARY_VERSION)
        return True
    else:
        unget_char(f, c)
        return False

FUTHARK_INT8 = 0
FUTHARK_INT16 = 1
FUTHARK_INT32 = 2
FUTHARK_INT64 = 3
FUTHARK_UINT8 = 4
FUTHARK_UINT16 = 5
FUTHARK_UINT32 = 6
FUTHARK_UINT64 = 7
FUTHARK_FLOAT32 = 8
FUTHARK_FLOAT64 = 9
FUTHARK_BOOL = 10


FUTHARK_PRIMTYPES = {}
FUTHARK_PRIMTYPES[FUTHARK_INT8] = \
    {'binname' : "  i8",
     'type_name' : "i8",
     'size' : 1,
     'bin_reader': read_bin_i8,
     'str_reader': read_str_i8
    }
FUTHARK_PRIMTYPES[FUTHARK_INT16]   = \
    {'binname' : " i16",
     'type_name' : "i16",
     'size' : 2,
     'bin_reader': read_bin_i16,
     'str_reader': read_str_i16
    }
FUTHARK_PRIMTYPES[FUTHARK_INT32]   = \
    {'binname' : " i32",
     'type_name' : "i32",
     'size' : 4,
     'bin_reader': read_bin_i32,
     'str_reader': read_str_i32
    }
FUTHARK_PRIMTYPES[FUTHARK_INT64]   = \
    {'binname' : " i64",
     'type_name' : "i64",
     'size' : 8,
     'bin_reader': read_bin_i64,
     'str_reader': read_str_i64
    }

FUTHARK_PRIMTYPES[FUTHARK_UINT8] = \
    {'binname' : "  i8",
     'type_name' : "u8",
     'size' : 1,
     'bin_reader': read_bin_u8,
     'str_reader': read_str_u8
    }
FUTHARK_PRIMTYPES[FUTHARK_UINT16]   = \
    {'binname' : " i16",
     'type_name' : "u16",
     'size' : 2,
     'bin_reader': read_bin_u16,
     'str_reader': read_str_u16
    }
FUTHARK_PRIMTYPES[FUTHARK_UINT32]   = \
    {'binname' : " i32",
     'type_name' : "u32",
     'size' : 4,
     'bin_reader': read_bin_u32,
     'str_reader': read_str_u32
    }
FUTHARK_PRIMTYPES[FUTHARK_UINT64]   = \
    {'binname' : " i64",
     'type_name' : "u64",
     'size' : 8,
     'bin_reader': read_bin_u64,
     'str_reader': read_str_u64
    }

FUTHARK_PRIMTYPES[FUTHARK_FLOAT32] = \
    {'binname' : " f32",
     'type_name' : "f32",
     'size' : 4,
     'bin_reader': read_bin_f32,
     'str_reader': read_str_f32
    }
FUTHARK_PRIMTYPES[FUTHARK_FLOAT64] = \
    {'binname' : " f64",
     'type_name' : "f64",
     'size' : 8,
     'bin_reader': read_bin_f64,
     'str_reader': read_str_f64
    }
FUTHARK_PRIMTYPES[FUTHARK_BOOL]    = \
    {'binname' : "bool",
     'type_name' : "bool",
     'size' : 1,
     'bin_reader': read_bin_bool,
     'str_reader': read_str_bool
    }

def read_bin_read_type_enum(f):
    read_binname = get_chars(f, 4)

    for (k,v) in FUTHARK_PRIMTYPES.items():
        if v['binname'] == read_binname:
            return k
    panic(1, "binary-input: Did not recognize the type '%s'.\n", read_binname)

def read_bin_ensure_scalar(f, expected_type):
  dims = read_bin_i8(f)

  if bin_dims != 0:
      panic(1, "binary-input: Expected scalar (0 dimensions), but got array with %i dimensions.\n", bin_dims)

  bin_type_enum = read_bin_read_type_enum(f)
  if bin_type_enum != expected_type:
      panic(1, "binary-input: Expected scalar of type %s but got scalar of type %s.\n",
            FUTHARK_PRIMTYPES[expected_type]['type_name'],
            FUTHARK_PRIMTYPES[bin_type_enum]['type_name'])

# ------------------------------------------------------------------------------
# General interface for reading Primitive Futhark Values
# ------------------------------------------------------------------------------

def read_general(f, ty):
    if read_is_binary(f):
        read_bin_ensure_scalar(ty)
        return FUTHARK_PRIMTYPES[ty]['bin_reader'](f)
    return FUTHARK_PRIMTYPES[ty]['str_reader'](f)

def read_i8(f):
    return read_general(f, FUTHARK_INT8)

def read_i16(f):
    return read_general(f, FUTHARK_INT16)

def read_i32(f):
    return read_general(f, FUTHARK_INT32)

def read_i64(f):
    return read_general(f, FUTHARK_INT64)

def read_u8(f):
    return read_general(f, FUTHARK_UINT8)

def read_u16(f):
    return read_general(f, FUTHARK_UINT16)

def read_u32(f):
    return read_general(f, FUTHARK_UINT32)

def read_u64(f):
    return read_general(f, FUTHARK_UINT64)

def read_f32(f):
    return read_general(f, FUTHARK_FLOAT32)

def read_f64(f):
    return read_general(f, FUTHARK_FLOAT64)

def read_bool(f):
    return read_general(f, FUTHARK_BOOL)

def read_array(f, expected_type, type_name, rank, ctype):
    if not read_is_binary(f):
        str_reader = FUTHARK_PRIMTYPES[expected_type]['str_reader']
        return read_str_array(f, str_reader, type_name, rank, ctype)

    bin_rank = read_bin_u8(f)

    if bin_rank != rank:
        panic(1, "binary-input: Expected %i dimensions, but got array with %i dimensions.\n",
              rank, bin_rank)

    bin_type_enum = read_bin_read_type_enum(f)
    if expected_type != bin_type_enum:
        panic(1, "binary-input: Expected %iD-array with element type '%s' but got %iD-array with element type '%s'.\n",
              rank, FUTHARK_PRIMTYPES[expected_type]['type_name'],
              rank, FUTHARK_PRIMTYPES[bin_type_enum]['type_name'])

    shape = []
    elem_count = 1
    for i in range(rank):
        bin_size = read_bin_u64(f)
        elem_count *= bin_size
        shape.append(bin_size)

    bin_reader = FUTHARK_PRIMTYPES[bin_type_enum]['bin_reader']

    arr = np.empty(elem_count, dtype=ctype)
    for i in range(elem_count):
        arr[i] = bin_reader(f)
    arr.shape = shape
    return arr

################################################################################
### end of reader.py
################################################################################
