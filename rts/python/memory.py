# Helper functions dealing with memory blocks.

import ctypes as ct

def addressOffset(x, offset, bt):
  offset = np.asscalar(offset)
  return ct.cast(ct.addressof(x.contents)+offset, ct.POINTER(bt))

def allocateMem(size):
  return ct.cast((ct.c_byte * max(0,size))(), ct.POINTER(ct.c_byte))

def unwrapArray(x):
  return x.ctypes.data_as(ct.POINTER(ct.c_byte))

def createArray(x, dim):
  return np.ctypeslib.as_array(x, shape=dim)

def indexArray(x, offset, bt, nptype):
  return nptype(addressOffset(x, offset, bt)[0])

def writeScalarArray(x, offset, v):
  offset = np.asscalar(offset)
  ct.memmove(ct.addressof(x.contents)+offset, ct.addressof(v), ct.sizeof(v))
