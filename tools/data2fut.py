#!/usr/bin/env python3
#
# Convert the contents of a file to a Futhark-compatible binary data
# file.  The file is interpreted as raw binary data corresponding to a
# one-dimensional array.
#
# Takes a single option: the name of the element type.  Only the
# numeric types are supported (no floats).
#
# Usage example:
#
# $ ./data2fut.py f32 INPUT OUTPUT

import futhark_data as fd
import numpy as np
import sys

types = {
    "f16": np.float16,
    "f32": np.float32,
    "f64": np.float64,
    "i8": np.int8,
    "i16": np.int16,
    "i32": np.int32,
    "i64": np.int64,
    "u8": np.uint8,
    "u16": np.uint16,
    "u32": np.uint32,
    "u64": np.uint64,
}

_, type, input, output = sys.argv
dtype = types[type]

v = np.fromfile(open(input, mode="rb"), dtype)
fd.dump(v, open(output, mode="wb"), binary=True)
