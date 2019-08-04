#!/usr/bin/env python3
#
# Turn a Futhark value of type [height][width]i32 encoded in the
# binary data format on stdin into a PNG image to the provided file.
# Absolutely no error checking is done.

import sys
import struct
import numpy as np
import png

def read_image(f = sys.stdin.buffer):
    # Skip binary header.
    f.read(1 # 'b'
           + 1 # version
           + 1 # rank
           + 4 # type
    )

    height = np.int64(struct.unpack('<Q', f.read(8))[0])
    width = np.int64(struct.unpack('<Q', f.read(8))[0])
    array = np.frombuffer(f.read(height*width*4), dtype=np.int32).reshape(height, width)

    image=np.empty((height,width,3), dtype=np.int8)
    image[:,:,0] = (array & 0xFF0000) >> 16
    image[:,:,1] = (array & 0xFF00) >> 8
    image[:,:,2] = (array & 0xFF)

    return (width, height, np.reshape(image, (height, width*3)))


if __name__ == '__main__':
    fname = sys.argv[1]
    width, height, img = read_image()
    with open(fname, 'wb') as f:
        w = png.Writer(width=width, height=height, alpha=False)
        w.write(f, img)
