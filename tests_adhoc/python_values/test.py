import io
from pathlib import Path
import runpy
import unittest


runtime = runpy.run_path(
    str(Path(__file__).resolve().parents[2] / "rts/python/values.py")
)


class EofLimitedInput(io.BytesIO):
    def __init__(self, data):
        super().__init__(data)
        self.eof_reads = 0

    def read(self, size=-1):
        data = super().read(size)
        if data == b"":
            self.eof_reads += 1
            if self.eof_reads > 100:
                raise AssertionError("Reader kept reading at EOF")
        return data


class ValuesTests(unittest.TestCase):
    def read_case(self, data, ty, expected):
        reader = runtime["ReaderInput"](EofLimitedInput(data))
        self.assertEqual(runtime["read_value"](ty, reader), expected)
        runtime["end_of_input"]("main", reader)

    def test_decimal_zero(self):
        for ty in (
            "i8",
            "i16",
            "i32",
            "i64",
            "u8",
            "u16",
            "u32",
            "u64",
            "f16",
            "f32",
            "f64",
        ):
            with self.subTest(ty=ty):
                self.read_case(b"0", ty, 0)

    def test_signed_zero(self):
        for ty in ("i8", "i16", "i32", "i64"):
            for data in (b"-0", b"+0"):
                with self.subTest(ty=ty, data=data):
                    self.read_case(data, ty, 0)

    def test_hexadecimal_integer(self):
        for ty in ("i8", "i16", "i32", "i64", "u8", "u16", "u32", "u64"):
            for data, expected in ((b"0x2a", 42), (b"0X2A", 42), (b"0x0", 0)):
                with self.subTest(ty=ty, data=data):
                    self.read_case(data, ty, expected)

    def test_trailing_comment(self):
        for data in (
            b"42 -- comment",
            b"42 --",
            b"42 --\n",
            b"42 -- comment\n",
            b"-- before\n42 -- after",
            b"-- before\n42",
        ):
            with self.subTest(data=data):
                self.read_case(data, "i32", 42)

    def test_empty_entry(self):
        for data in (b"", b" \t", b"-- comment", b"--", b"--\n"):
            with self.subTest(data=data):
                reader = runtime["ReaderInput"](EofLimitedInput(data))
                runtime["end_of_input"]("main", reader)

    def test_controls(self):
        for data, expected in (
            (b"42", 42),
            (b"42i32", 42),
            (b"0i32", 0),
            (b"0\n", 0),
            (b"0x2ai32", 42),
            (b"0x2a\n", 42),
            (b"0X2Ai32", 42),
            (b"-42", -42),
        ):
            with self.subTest(data=data):
                self.read_case(data, "i32", expected)

    def test_invalid_input(self):
        for data in (b"", b"0x", b"0X", b"0x_", b"-- only comment", b"--"):
            with self.subTest(data=data):
                reader = runtime["ReaderInput"](EofLimitedInput(data))
                with self.assertRaises(ValueError):
                    runtime["read_value"]("i32", reader)

    def test_comment_between_values(self):
        reader = runtime["ReaderInput"](
            EofLimitedInput(b"42 -- comment\n0x2a")
        )
        self.assertEqual(runtime["read_value"]("i32", reader), 42)
        self.assertEqual(runtime["read_value"]("i32", reader), 42)
        runtime["end_of_input"]("main", reader)

    def test_array_with_trailing_comment(self):
        reader = runtime["ReaderInput"](
            EofLimitedInput(b"[0, 0x2a] -- comment")
        )
        self.assertEqual(
            runtime["read_value"]("[]i32", reader).tolist(), [0, 42]
        )
        runtime["end_of_input"]("main", reader)


if __name__ == "__main__":
    unittest.main()
