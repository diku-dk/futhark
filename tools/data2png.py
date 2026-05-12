#!/usr/bin/env python3
#
# Turn a Futhark value encoded in the binary data format in some file
# into a PNG image in another file.  Very little error checking is
# done.  The following input types are supported:
#
#  [height][width]u8
#  [height][width]i32
#  [height][width]u32
#
#  [height][width]f32
#  [height][width]f64
#
#  [height][width][3]i8
#  [height][width][3]u8
#
# Requires purepng and Numpy.
#
# Example:
#
# $ cat input | ./some-futhark-program | tools/data2png.py /dev/stdin out.png

import sys
import struct
import numpy as np
import png

if "purepng" not in png.__file__:
    print("data2png works only with purepng, not pypng.", file=sys.stderr)
    sys.exit(1)


def read_image(f):
    # Skip binary header.
    assert f.read(1) == b"b", "Invalid binary header"
    assert f.read(1) == b"\2", "Invalid binary format"
    rank = np.int8(struct.unpack("<B", f.read(1))[0])
    type = f.read(4)

    if rank == 2 and type == b"  u8":
        height = np.int64(struct.unpack("<Q", f.read(8))[0])
        width = np.int64(struct.unpack("<Q", f.read(8))[0])
        array = np.frombuffer(f.read(height * width), dtype=np.uint8).reshape(
            height, width
        )

        return (width, height, array)

    if rank == 2 and type in [b" i32", b" u32", b" f32", b" f64"]:
        height = np.int64(struct.unpack("<Q", f.read(8))[0])
        width = np.int64(struct.unpack("<Q", f.read(8))[0])

        image = np.empty((height, width, 3), dtype=np.uint8)
        if type == b" f32":
            array = np.frombuffer(
                f.read(height * width * 4), dtype=np.float32
            ).reshape(height, width)
            image[:, :, 0] = image[:, :, 1] = image[:, :, 2] = np.uint8(
                np.float32(array) * 255
            )
        if type == b" f64":
            array = np.frombuffer(
                f.read(height * width * 8), dtype=np.float64
            ).reshape(height, width)
            image[:, :, 0] = image[:, :, 1] = image[:, :, 2] = np.uint8(
                np.float64(array) * 255
            )
        else:
            array = np.frombuffer(
                f.read(height * width * 4), dtype=np.uint32
            ).reshape(height, width)
            image[:, :, 0] = (array & 0xFF0000) >> 16
            image[:, :, 1] = (array & 0xFF00) >> 8
            image[:, :, 2] = array & 0xFF

        return (width, height, np.reshape(image, (height, width * 3)))

    elif rank == 3 and type in [b"  i8", b"  u8"]:
        height = np.int64(struct.unpack("<Q", f.read(8))[0])
        width = np.int64(struct.unpack("<Q", f.read(8))[0])
        depth = np.int64(struct.unpack("<Q", f.read(8))[0])
        assert depth == 3
        image = np.frombuffer(
            f.read(height * width * depth), dtype=np.uint8
        ).reshape(height, width, depth)

        return (width, height, np.reshape(image, (height, width * 3)))

    else:
        raise Exception(
            "Invalid rank '{}' and element type {}".format(rank, type)
        )


if __name__ == "__main__":
    infile = sys.argv[1]
    outfile = sys.argv[2]

    with open(infile, "rb") as f_in:
        width, height, img = read_image(f_in)
        with open(outfile, "wb") as f_out:
            w = png.Writer(width=width, height=height, alpha=False)
            w.write(f_out, img)
