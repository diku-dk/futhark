# Start of memory.py.

import ctypes as ct


def addressOffset(x, offset, bt):
    return ct.cast(ct.addressof(x.contents) + int(offset), ct.POINTER(bt))


def allocateMem(size):
    return ct.cast((ct.c_byte * max(0, size))(), ct.POINTER(ct.c_byte))


# Copy an array if its is not-None.  This is important for treating
# Numpy arrays as flat memory, but has some overhead.
def normaliseArray(x):
    if (x.base is x) or (x.base is None):
        return x
    else:
        return x.copy()


def unwrapArray(x):
    return normaliseArray(x).ctypes.data_as(ct.POINTER(ct.c_byte))


def createArray(x, shape, t):
    # HACK: np.ctypeslib.as_array may fail if the shape contains zeroes,
    # for some reason.
    if any(map(lambda x: x == 0, shape)):
        return np.ndarray(shape, dtype=t)
    else:
        return np.ctypeslib.as_array(x, shape=shape).view(t)


def indexArray(x, offset, bt):
    return addressOffset(x, offset * ct.sizeof(bt), bt)[0]


def writeScalarArray(x, offset, v):
    ct.memmove(
        ct.addressof(x.contents) + int(offset) * ct.sizeof(v),
        ct.addressof(v),
        ct.sizeof(v),
    )


# An opaque Futhark value.
class opaque(object):
    def __init__(self, desc, *payload):
        self.data = payload
        self.desc = desc

    def __repr__(self):
        return "<opaque Futhark value of type {}>".format(self.desc)


# LMAD stuff


def lmad_contiguous_search(checked, expected, strides, shape, used):
    for i in range(len(strides)):
        for j in range(len(strides)):
            if not used[j] and strides[j] == expected and strides[j] >= 0:
                used[j] = True
                if checked + 1 == len(strides) or lmad_contiguous_search(
                    checked + 1, expected * shape[j], strides, shape, used
                ):
                    return True
                used[j] = False
    return False


def lmad_contiguous(strides, shape):
    used = len(strides) * [False]
    return lmad_contiguous_search(0, 1, strides, shape, used)


def lmad_memcpyable(dst_strides, src_strides, shape):
    if not lmad_contiguous(dst_strides, shape):
        return False
    for i in range(len(dst_strides)):
        if dst_strides[i] != src_strides[i] and shape[i] != 1:
            return False
    return True


def lmad_copy_elements(
    pt, dst, dst_offset, dst_strides, src, src_offset, src_strides, shape
):
    if len(shape) == 1:
        for i in range(shape[0]):
            writeScalarArray(
                dst,
                dst_offset + i * dst_strides[0],
                pt(indexArray(src, src_offset + i * src_strides[0], pt)),
            )
    else:
        for i in range(shape[0]):
            lmad_copy_elements(
                pt,
                dst,
                dst_offset + i * dst_strides[0],
                dst_strides[1:],
                src,
                src_offset + i * src_strides[0],
                src_strides[1:],
                shape[1:],
            )


def lmad_copy(
    pt, dst, dst_offset, dst_strides, src, src_offset, src_strides, shape
):
    if lmad_memcpyable(dst_strides, src_strides, shape):
        ct.memmove(
            addressOffset(dst, dst_offset * ct.sizeof(pt), ct.c_byte),
            addressOffset(src, src_offset * ct.sizeof(pt), ct.c_byte),
            np.prod(shape) * ct.sizeof(pt),
        )
    else:
        lmad_copy_elements(
            pt,
            dst,
            dst_offset,
            dst_strides,
            src,
            src_offset,
            src_strides,
            shape,
        )


# End of memory.py.
