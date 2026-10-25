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
#
# Behaves like a tuple, even for opaques that are not really tuple-like.
# Beware...
class opaque(object):
    def __init__(self, desc, opaques, *payload):
        self.data = payload
        self.opaques = opaques
        self.desc = desc

    def __len__(self):
        return len(self.opaques[self.desc][1])

    # Return number of Python values used to represent Futhark value of given
    # type.
    def __num_elems(self, t):
        if t in self.opaques:
            return sum(map(self.__num_elems, self.opaques[t][0]))
        else:
            return 1

    def __getitem__(self, i):
        layout = self.opaques[self.desc][1]
        if layout is not None:
            if i < 0 or i >= len(layout):
                raise IndexError
            ks = list(map(self.__num_elems, layout))
            t = layout[i]
            if t in self.opaques:
                return opaque(
                    layout[i], self.opaques, *self.data[sum(ks[0:i]) : ks[i]]
                )
            else:
                return self.data[sum(ks[0:i])]
        else:
            raise TypeError

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


# See lmad_map_tr() in rts/c/copy.h.  Returns a tuple (num_arrays,
# rows, cols, dst_array_stride, dst_row_stride, src_array_stride,
# src_row_stride), or None if the copy is not a map(transpose).
def lmad_map_tr(dst_strides, src_strides, shape):
    c_shape = []
    c_dst_strides = []
    c_src_strides = []
    for d, s, n in zip(dst_strides, src_strides, shape):
        if n == 1:
            continue
        if (
            c_shape
            and c_dst_strides[-1] == d * n
            and c_src_strides[-1] == s * n
        ):
            c_shape[-1] *= n
            c_dst_strides[-1] = d
            c_src_strides[-1] = s
        else:
            c_shape.append(n)
            c_dst_strides.append(d)
            c_src_strides.append(s)

    if len(c_shape) not in (2, 3):
        return None

    a, b = len(c_shape) - 2, len(c_shape) - 1
    if c_dst_strides[b] == 1 and c_src_strides[a] == 1:
        rows, cols = c_shape[b], c_shape[a]
        dst_row_stride, src_row_stride = c_dst_strides[a], c_src_strides[b]
    elif c_dst_strides[a] == 1 and c_src_strides[b] == 1:
        rows, cols = c_shape[a], c_shape[b]
        dst_row_stride, src_row_stride = c_dst_strides[b], c_src_strides[a]
    else:
        return None

    if len(c_shape) == 3:
        k, dst_array_stride, src_array_stride = (
            c_shape[0],
            c_dst_strides[0],
            c_src_strides[0],
        )
    else:
        k, dst_array_stride, src_array_stride = 1, 0, 0

    return (
        k,
        rows,
        cols,
        dst_array_stride,
        dst_row_stride,
        src_array_stride,
        src_row_stride,
    )


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
