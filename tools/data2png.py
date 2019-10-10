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

def read_image(f):
    # Skip binary header.
    assert f.read(1) == b'b', 'Invalid binary header'
    assert f.read(1) == b'\2', 'Invalid binary format'
    rank = np.int8(struct.unpack('<B', f.read(1))[0])
    type = f.read(4)

    if rank == 2 and type == b'  u8':
        height = np.int64(struct.unpack('<Q', f.read(8))[0])
        width = np.int64(struct.unpack('<Q', f.read(8))[0])
        array = np.frombuffer(f.read(height*width), dtype=np.uint8).reshape(height, width)

        return (width, height, array)

    if rank == 2 and type in [b' i32', b' u32', b' f32']:
        height = np.int64(struct.unpack('<Q', f.read(8))[0])
        width = np.int64(struct.unpack('<Q', f.read(8))[0])

        image=np.empty((height,width,3), dtype=np.uint8)
        if type == b' f32':
            array = np.frombuffer(f.read(height*width*4), dtype=np.float32).reshape(height, width)
            image[:,:,0] = image[:,:,1] = image[:,:,2] = np.uint8(np.float32(array)*256)
        else:
            array = np.frombuffer(f.read(height*width*4), dtype=np.uint32).reshape(height, width)
            image[:,:,0] = (array & 0xFF0000) >> 16
            image[:,:,1] = (array & 0xFF00) >> 8
            image[:,:,2] = (array & 0xFF)

        return (width, height, np.reshape(image, (height, width*3)))

    elif rank == 3 and type in [b'  i8', b'  u8']:
        height = np.int64(struct.unpack('<Q', f.read(8))[0])
        width = np.int64(struct.unpack('<Q', f.read(8))[0])
        depth = np.int64(struct.unpack('<Q', f.read(8))[0])
        assert(depth == 3)
        image = np.frombuffer(f.read(height*width*depth), dtype=np.uint8).reshape(height, width, depth)

        return (width, height, np.reshape(image, (height, width*3)))

    else:
        raise Exception('Invalid rank \'{}\' and element type {}'.format(rank, type))

if __name__ == '__main__':
    infile = sys.argv[1]
    outfile = sys.argv[2]

    with open(infile, 'rb') as f:
        width, height, img = read_image(f)

        with open(outfile, 'wb') as f:
            w = png.Writer(width=width, height=height, alpha=False)
            w.write(f, img)
