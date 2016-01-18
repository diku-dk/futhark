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
def shl32(x,y):
  return x << y

def ashr32(x,y):
  return x >> y

def sdiv32(x,y):
  return x / y

def smod32(x,y):
  return x % y

def squot32(x,y):
  return int32(float(x) / float(y))

def srem32(x,y):
  return fmod(x,y)

def spow32(x,y):
  return x ** y

def fpow32(x,y):
  return x ** y

def sle32(x,y):
  return x <= y

def slt32(x,y):
  return x < y

def sitofp_i32_f32(x):
  return float32(x)

def sitofp_i32_f64(x):
  return float32(x)

def fptosi_f32_i32(x):
  return int32(trunc(x))

def fptosi_f64_i32(x):
  return int32(trunc(x))

def futhark_sqrt64(x):
  return sqrt(x)

def futhark_exp64(x):
  return exp(x)

def futhark_fact(x):
  return int32(math.factorial(x))

def futhark_trunc64(x):
  return int64(trunc(x))

def futhark_log32(x):
  return float32(log(x))

def futhark_sqrt32(x):
  return float32(sqrt(x))

def futhark_exp32(x):
  return exp(x)

def futhark_trunc32(x):
  return float32(trunc(x))
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
    return float(sign + bef + '.' + aft + 'E' + expt)

def read_float(f):
    return read_double(f)

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
