{-# LANGUAGE QuasiQuotes #-}

module Futhark.CodeGen.Backends.GenericPython.Definitions
  ( pyFunctions
    ,pyUtility
    ,pyTestMain
  ) where

import Futhark.CodeGen.Backends.GenericPython.AST

import Text.RawString.QQ

pyFunctions :: PyDefinition
pyFunctions = [r|
def addressOffset(x, offset, bt):
  offset = asscalar(offset)
  return cast(addressof(x.contents)+offset, POINTER(bt))

def allocateMem(size):
  return cast((c_byte * size)(), POINTER(c_byte))

def unwrapArray(x):
  return x.ctypes.data_as(POINTER(c_byte))

def createArray(x, dim):
  return ctypeslib.as_array(x, shape=dim)

def indexArray(x, offset, bt, nptype):
  return nptype(addressOffset(x, offset, bt)[0])

def writeScalarArray(x, offset, v):
  offset = asscalar(offset)
  memmove(addressof(x.contents)+offset, addressof(v), sizeof(v))
|]

pyUtility :: PyDefinition
pyUtility = [r|
def signed(x):
  if type(x) == uint8:
    return int8(x)
  elif type(x) == uint16:
    return int16(x)
  elif type(x) == uint32:
    return int32(x)
  else:
    return int64(x)

def unsigned(x):
  if type(x) == int8:
    return uint8(x)
  elif type(x) == int16:
    return uint16(x)
  elif type(x) == int32:
    return uint32(x)
  else:
    return uint64(x)

def shlN(x,y):
  return x << y

def ashrN(x,y):
  return x >> y

def sdivN(x,y):
  return x / y

def smodN(x,y):
  return x % y

def udivN(x,y):
  return signed(unsigned(x) / unsigned(y))

def umodN(x,y):
  return signed(unsigned(x) % unsigned(y))

def squotN(x,y):
  return int32(float(x) / float(y))

def sremN(x,y):
  return fmod(x,y)

def powN(x,y):
  return x ** y

def fpowN(x,y):
  return x ** y

def sleN(x,y):
  return x <= y

def sltN(x,y):
  return x < y

def uleN(x,y):
  return unsigned(x) <= unsigned(y)

def ultN(x,y):
  return unsigned(x) < unsigned(y)

def lshr8(x,y):
  return int8(uint8(x) >> uint8(y))

def lshr16(x,y):
  return int16(uint16(x) >> uint16(y))

def lshr32(x,y):
  return int32(uint32(x) >> uint32(y))

def lshr64(x,y):
  return int64(uint64(x) >> uint64(y))

def sext_T_i8(x):
  return int8(x)

def sext_T_i16(x):
  return int16(x)

def sext_T_i32(x):
  return int32(x)

def sext_T_i64(x):
  return int32(x)

def zext_i8_i8(x):
  return int8(uint8(x))

def zext_i8_i16(x):
  return int16(uint8(x))

def zext_i8_i32(x):
  return int32(uint8(x))

def zext_i8_i64(x):
  return int64(uint8(x))

def zext_i16_i8(x):
  return int8(uint16(x))

def zext_i16_i16(x):
  return int16(uint16(x))

def zext_i16_i32(x):
  return int32(uint16(x))

def zext_i16_i64(x):
  return int64(uint16(x))

def zext_i32_i8(x):
  return int8(uint32(x))

def zext_i32_i16(x):
  return int16(uint32(x))

def zext_i32_i32(x):
  return int32(uint32(x))

def zext_i32_i64(x):
  return int64(uint32(x))

def zext_i64_i8(x):
  return int8(uint64(x))

def zext_i64_i16(x):
  return int16(uint64(x))

def zext_i64_i32(x):
  return int32(uint64(x))

def zext_i64_i64(x):
  return int64(uint64(x))

shl8 = shl16 = shl32 = shl64 = shlN
ashr8 = ashr16 = ashr32 = ashr64 = ashrN
sdiv8 = sdiv16 = sdiv32 = sdiv64 = sdivN
smod8 = smod16 = smod32 = smod64 = smodN
udiv8 = udiv16 = udiv32 = udiv64 = udivN
umod8 = umod16 = umod32 = umod64 = umodN
squot8 = squot16 = squot32 = squot64 = squotN
srem8 = srem16 = srem32 = srem64 = sremN
pow8 = pow16 = pow32 = pow64 = powN
fpow32 = fpow64 = fpowN
sle8 = sle16 = sle32 = sle64 = sleN
slt8 = slt16 = slt32 = slt64 = sltN
ule8 = ule16 = ule32 = ule64 = uleN
ult8 = ult16 = ult32 = ult64 = ultN
sext_i8_i8 = sext_i16_i8 = sext_i32_i8 = sext_i64_i8 = sext_T_i8
sext_i8_i16 = sext_i16_i16 = sext_i32_i16 = sext_i64_i16 = sext_T_i16
sext_i8_i32 = sext_i16_i32 = sext_i32_i32 = sext_i64_i32 = sext_T_i32
sext_i8_i64 = sext_i16_i64 = sext_i32_i64 = sext_i64_i64 = sext_T_i64

def ssignum(x):
  return sign(x)

def usignum(x):
  if x < 0:
    return ssignum(-x)
  else:
    return ssignum(x)

def sitofp_T_f32(x):
  return float32(x)
sitofp_i8_f32 = sitofp_i16_f32 = sitofp_i32_f32 = sitofp_i64_f32 = sitofp_T_f32

def sitofp_T_f64(x):
  return float64(x)
sitofp_i8_f64 = sitofp_i16_f64 = sitofp_i32_f64 = sitofp_i64_f64 = sitofp_T_f64

def uitofp_T_f32(x):
  return float32(unsigned(x))
uitofp_i8_f32 = uitofp_i16_f32 = uitofp_i32_f32 = uitofp_i64_f32 = uitofp_T_f32

def uitofp_T_f64(x):
  return float64(unsigned(x))
uitofp_i8_f64 = uitofp_i16_f64 = uitofp_i32_f64 = uitofp_i64_f64 = uitofp_T_f64

def fptosi_T_i8(x):
  return int8(trunc(x))
fptosi_f32_i8 = fptosi_f64_i8 = fptosi_T_i8

def fptosi_T_i16(x):
  return int16(trunc(x))
fptosi_f32_i16 = fptosi_f64_i16 = fptosi_T_i16

def fptosi_T_i32(x):
  return int32(trunc(x))
fptosi_f32_i32 = fptosi_f64_i32 = fptosi_T_i32

def fptosi_T_i64(x):
  return int64(trunc(x))
fptosi_f32_i64 = fptosi_f64_i64 = fptosi_T_i64

def fptoui_T_i8(x):
  return uint8(trunc(x))

def fptoui_T_i16(x):
  return uint16(trunc(x))

def fptoui_T_i32(x):
  return uint32(trunc(x))

def fptoui_T_i64(x):
  return uint64(trunc(x))

fptoui_f32_i64 = fptoui_f64_i64 = fptoui_T_i64

def fpconv_f32_f64(x):
  return float64(x)

def fpconv_f64_f32(x):
  return float32(x)

def futhark_log64(x):
  return float64(log(x))

def futhark_sqrt64(x):
  return sqrt(x)

def futhark_exp64(x):
  return exp(x)

def futhark_cos64(x):
  return cos(x)

def futhark_sin64(x):
  return sin(x)

def futhark_log32(x):
  return float32(log(x))

def futhark_sqrt32(x):
  return float32(sqrt(x))

def futhark_exp32(x):
  return exp(x)

def futhark_cos32(x):
  return cos(x)

def futhark_sin32(x):
  return sin(x)
|]

pyTestMain :: PyDefinition
pyTestMain = [r|
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
    while c != None:
        if c.isdigit():
            s += c
            c = get_char(f)
        else:
            unget_char(f, c)
            break
    optional(read_int_trailer, f)
    return s

def parse_int_signed(f):
    s = ''
    c = get_char(f)

    if c == '-' and peek_char(f).isdigit():
      s = c + parse_int(f)
    else:
      unget_char(f, c)
      s = parse_int(f)

    return s

def read_int_trailer(f):
  parse_specific_char(f, 'i')
  while peek_char(f).isdigit():
    get_char(f)

def read_comma(f):
    skip_spaces(f)
    parse_specific_char(f, ',')
    return ','

def read_int(f):
    skip_spaces(f)
    return int(parse_int_signed(f))

def read_char(f):
    skip_spaces(f)
    parse_specific_char(f, '\'')
    c = get_char(f)
    parse_specific_char(f, '\'')
    return c

def read_double(f):
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
    optional(read_float_trailer, f)
    return float(sign + bef + '.' + aft + 'E' + expt)

def read_float(f):
    return read_double(f)

def read_float_trailer(f):
  parse_specific_char(f, 'f')
  while peek_char(f).isdigit():
    get_char(f)

def read_bool(f):
    skip_spaces(f)
    if peek_char(f) == 'T':
        parse_specific_string(f, 'True')
        return True
    elif peek_char(f) == 'F':
        parse_specific_string(f, 'False')
        return False
    else:
        raise ValueError

def read_array_elems(f, elem_reader):
    skip_spaces(f)
    parse_specific_char(f, '[')
    xs = sepBy(elem_reader, read_comma, f)
    skip_spaces(f)
    parse_specific_char(f, ']')
    return xs

def read_array_helper(f, elem_reader, rank):
    def nested_row_reader(_):
        return read_array_helper(f, elem_reader, rank-1)
    if rank == 1:
        row_reader = elem_reader
    else:
        row_reader = nested_row_reader
    return read_array_elems(f, row_reader)

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

def read_double_signed(f):

    skip_spaces(f)
    c = get_char(f)

    if c == '-' and peek_char(f).isdigit():
      v = -1 * read_double(f)
    else:
      unget_char(f, c)
      v = read_double(f)

    return v

def read_array(f, elem_reader, rank, bt):
    elems = read_array_helper(f, elem_reader, rank)
    dims = expected_array_dims(elems, rank)
    verify_array_dims(elems, dims)
    return array(elems, dtype=bt)

def write_chars(f, arr):
    f.write("\"")
    for x in arr:
      f.write(x.decode())
    f.write("\"")

def write_array(f, arr, bt):
    if arr.size == 0:
        print("empty({})".format(bt))
    else:
        print(arr.tolist())
|]
