#!/usr/bin/env python3
#
# Turn a Futhark value encoded in the binary data format on stdin into
# a PNG image to the provided file.  Very little error checking is
# done.  The following input types are supported:
#
#  [height][width]i32
#
#  [height][width][3]i8
#
#
# Requires purepng and Numpy.

import sys
import struct
import numpy as np
import png

def read_image(f = sys.stdin.buffer):
    # Skip binary header.
    assert f.read(1) == b'b', 'Invalid binary header'
    assert f.read(1) == b'\2', 'Invalid binary format'
    rank = np.int8(struct.unpack('<B', f.read(1))[0])
    type = f.read(4)

    if rank == 2:
        assert type in [b' i32', b' u32'], 'Invalid element type {}'.format(type)
        height = np.int64(struct.unpack('<Q', f.read(8))[0])
        width = np.int64(struct.unpack('<Q', f.read(8))[0])
        array = np.frombuffer(f.read(height*width*4), dtype=np.int32).reshape(height, width)

        image=np.empty((height,width,3), dtype=np.int8)
        image[:,:,0] = (array & 0xFF0000) >> 16
        image[:,:,1] = (array & 0xFF00) >> 8
        image[:,:,2] = (array & 0xFF)

        return (width, height, np.reshape(image, (height, width*3)))

    elif rank == 3:
        assert type in [b'  i8', b'  u8'], 'Invalid element type {}'.format(type)
        height = np.int64(struct.unpack('<Q', f.read(8))[0])
        width = np.int64(struct.unpack('<Q', f.read(8))[0])
        depth = np.int64(struct.unpack('<Q', f.read(8))[0])
        assert(depth == 3)
        image = np.frombuffer(f.read(height*width*depth), dtype=np.uint8).reshape(height, width, depth)

        return (width, height, np.reshape(image, (height, width*3)))

    else:
        raise Error('Invalid input rank {}'.format(rank))

if __name__ == '__main__':
    fname = sys.argv[1]
    width, height, img = read_image()
    with open(fname, 'wb') as f:
        w = png.Writer(width=width, height=height, alpha=False)
        w.write(f, img)
