"""Exercise benchmark metadata through the real ``futhark bench`` CLI."""

from __future__ import annotations

from datetime import datetime
import json
import os
from pathlib import Path
import subprocess
import tempfile
import unittest


class BenchMetadataTests(unittest.TestCase):
    def test_metadata_describes_invocation(self):
        with tempfile.TemporaryDirectory() as temp:
            root = Path(temp)
            program = root / "metadata.fut"
            results = root / "results.json"
            program.write_text(
                "-- ==\n-- input { 1 } output { 2 }\n\n"
                "entry main (x: i32) = x + 1\n",
                encoding="utf-8",
            )
            futhark = os.environ.get("FUTHARK", "futhark")
            run = subprocess.run(
                [
                    futhark,
                    "bench",
                    "--backend=c",
                    "--runs=1",
                    "--no-convergence-phase",
                    "--no-tuning",
                    "--pass-compiler-option=--safe",
                    "--pass-option=--debugging",
                    f"--json={results}",
                    str(program),
                ],
                cwd=root,
                capture_output=True,
                text=True,
            )
            self.assertEqual(run.returncode, 0, run.stderr)

            benchmarks = json.loads(results.read_text(encoding="utf-8"))
            self.assertEqual(len(benchmarks), 1)
            benchmark = next(iter(benchmarks.values()))
            self.assertIn("datasets", benchmark)
            metadata = benchmark["metadata"]
            self.assertEqual(metadata["backend"], "c")
            self.assertEqual(metadata["compiler_options"], ["--safe"])
            self.assertEqual(metadata["runtime_options"], ["--debugging"])
            self.assertTrue(metadata["hostname"])
            self.assertIn("Futhark", metadata["compiler_version"])
            start = datetime.fromisoformat(
                metadata["start_time"].replace("Z", "+00:00")
            )
            end = datetime.fromisoformat(
                metadata["end_time"].replace("Z", "+00:00")
            )
            self.assertLessEqual(start, end)

            compare = subprocess.run(
                [futhark, "benchcmp", str(results), str(results)],
                cwd=root,
                capture_output=True,
                text=True,
            )
            self.assertEqual(compare.returncode, 0, compare.stderr)


if __name__ == "__main__":
    unittest.main()
