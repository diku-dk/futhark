"""Regression test for tools/png2data.py.

png2data.py used to build its output array with np.empty() and then OR
the RGB channels into it. Two bugs followed from that:

1. np.empty() leaves the array's memory uninitialized, so leftover
   garbage from a previous allocation gets OR'ed into the pixel value
   instead of being overwritten.
2. OR-ing the (signed) int32 array with the (unsigned) uint32 channel
   data makes NumPy silently promote the result to int64, so the
   written payload is twice the size the " i32" header claims it is,
   corrupting every file the tool produces.

This fakes the `png` module (so it does not depend on pypng being
installed) through a shim module placed on PYTHONPATH, then runs the
real tools/png2data.py script through a subprocess on a tiny,
deterministic 1x2 image.
"""

from __future__ import annotations

import os
from pathlib import Path
import struct
import subprocess
import sys
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "tools" / "png2data.py"

HEIGHT = 1
WIDTH = 2
PIXELS = [[(10, 20, 30), (1, 2, 3)]]

FAKE_PNG_MODULE = f"""
class Reader:
    def __init__(self, path):
        pass

    def read(self):
        rows = {[[c for pixel in row for c in pixel] for row in PIXELS]!r}
        return ({WIDTH}, {HEIGHT}, rows, None)
"""


class Png2DataTests(unittest.TestCase):
    def test_i32_output_is_correct_and_not_corrupted(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            (root / "png.py").write_text(FAKE_PNG_MODULE, encoding="utf-8")
            out_path = root / "out.data"

            env = dict(os.environ)
            env["PYTHONPATH"] = (
                str(root) + os.pathsep + env.get("PYTHONPATH", "")
            )

            run = subprocess.run(
                [sys.executable, str(SCRIPT), "dummy_in.png", str(out_path)],
                cwd=root,
                env=env,
                capture_output=True,
                text=True,
            )
            self.assertEqual(run.returncode, 0, run.stderr)

            data = out_path.read_bytes()
            self.assertEqual(data[0:1], b"b")
            self.assertEqual(data[1:2], b"\x02")
            self.assertEqual(data[2], 2)
            self.assertEqual(data[3:7], b" i32")

            got_height, got_width = struct.unpack("<QQ", data[7:23])
            self.assertEqual((got_height, got_width), (HEIGHT, WIDTH))

            payload = data[23:]
            self.assertEqual(
                len(payload),
                HEIGHT * WIDTH * 4,
                f"payload is {len(payload)} bytes, expected "
                f"{HEIGHT * WIDTH * 4} for an i32 array of this shape",
            )

            values = struct.unpack("<" + "i" * (HEIGHT * WIDTH), payload)
            expected = [
                (r << 16) | (g << 8) | b for row in PIXELS for (r, g, b) in row
            ]
            self.assertEqual(list(values), expected)


if __name__ == "__main__":
    unittest.main()
