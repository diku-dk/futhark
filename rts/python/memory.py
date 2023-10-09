# Start of memory.py.

import ctypes as ct


def allocateMem(size):
    return np.empty(size, dtype=np.byte)


# Copy an array if its is not-None.  This is important for treating
# Numpy arrays as flat memory, but has some overhead.
def normaliseArray(x):
    if (x.base is x) or (x.base is None):
        return x
    else:
        return x.copy()


def unwrapArray(x):
    return x.ravel().view(np.byte)


def indexArray(x, offset, bt):
    return x.view(bt)[offset]


def writeScalarArray(x, offset, v):
    x.view(type(v))[offset] = v


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


def lmad_is_tr(strides, shape):
    r = len(shape)
    for i in range(1, r):
        n = 1
        m = 1
        ok = True
        expected = 1
        # Check strides before 'i'.
        for j in range(i - 1, -1, -1):
            ok = ok and strides[j] == expected
            expected *= shape[j]
            n *= shape[j]
        # Check strides after 'i'.
        for j in range(r - 1, i - 1, -1):
            ok = ok and strides[j] == expected
            expected *= shape[j]
            m *= shape[j]
        if ok:
            return (n, m)
    return None


def lmad_map_tr(dst_strides, src_strides, shape):
    r = len(dst_strides)
    rowmajor_strides = [0] * r
    rowmajor_strides[r - 1] = 1

    for i in range(r - 2, -1, -1):
        rowmajor_strides[i] = rowmajor_strides[i + 1] * shape[i + 1]

    # map_r will be the number of mapped dimensions on top.
    map_r = 0
    k = 1
    for i in range(r):
        if (
            dst_strides[i] != rowmajor_strides[i]
            or src_strides[i] != rowmajor_strides[i]
        ):
            break
        else:
            k *= shape[i]
            map_r += 1

    if rowmajor_strides[map_r:] == dst_strides[map_r:]:
        r = lmad_is_tr(src_strides[map_r:], shape[map_r:])
        if r is not None:
            (n, m) = r
            return (k, n, m)
    elif rowmajor_strides[map_r:] == src_strides[map_r:]:
        r = lmad_is_tr(dst_strides[map_r:], shape[map_r:])
        if r is not None:
            (n, m) = r
            return (k, m, n)  # Sic!
    return None


def lmad_copy_elements(
    pt, dst, dst_offset, dst_strides, src, src_offset, src_strides, shape
):
    if len(shape) == 1:
        for i in range(shape[0]):
            writeScalarArray(
                dst,
                dst_offset + i * dst_strides[0],
                indexArray(src, src_offset + i * src_strides[0], pt),
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
        dst[
            dst_offset * ct.sizeof(pt) : dst_offset * ct.sizeof(pt)
            + np.prod(shape) * ct.sizeof(pt)
        ] = src[
            src_offset * ct.sizeof(pt) : src_offset * ct.sizeof(pt)
            + np.prod(shape) * ct.sizeof(pt)
        ]
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
