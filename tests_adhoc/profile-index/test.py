"""Exercise profile navigation using benchmark JSON, without a GPU."""

from __future__ import annotations

import json
from pathlib import Path
import subprocess
import tempfile
import unittest
from html.parser import HTMLParser
from urllib.parse import unquote, urlsplit


class Page(HTMLParser):
    def __init__(self, path: Path):
        super().__init__()
        self.links: list[tuple[str, str]] = []
        self.tags: list[str] = []
        self.link: str | None = None
        self.label = ""
        self.feed(path.read_text())

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]):
        self.tags.append(tag)
        if tag == "a":
            self.link = dict(attrs)["href"]
            self.label = ""

    def handle_data(self, data: str):
        if self.link is not None:
            self.label += data

    def handle_endtag(self, tag: str):
        if tag == "a" and self.link is not None:
            self.links.append((self.link, self.label))
            self.link = None


def result(profiling=True):
    value = {"runtimes": [10], "bytes": {}, "stderr": "synthetic log"}
    if profiling:
        value["profiling"] = {
            "memory": {},
            "events": [
                {
                    "name": "synthetic kernel",
                    "duration": 10,
                    "provenance": "unknown",
                    "details": {},
                }
            ],
        }
    return value


class ProfileIndexTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()

    def profile(self, programs):
        input_path = self.root / "results.json"
        input_path.write_text(json.dumps(programs))
        run = subprocess.run(
            ["futhark", "profile", str(input_path)],
            cwd=self.root,
            capture_output=True,
            text=True,
            check=True,
        )
        return self.root / "results.prof", run.stderr

    def links(self, path):
        page = Page(path)
        self.assertNotIn("script", page.tags)
        resolved = {}
        for href, label in page.links:
            url = urlsplit(href)
            self.assertFalse(
                url.scheme or url.netloc or url.query or url.fragment
            )
            self.assertFalse(Path(unquote(url.path)).is_absolute())
            target = (path.parent / unquote(url.path)).resolve()
            self.assertTrue(target.is_relative_to(self.root))
            self.assertTrue(target.is_file(), f"Broken link: {path} -> {href}")
            self.assertNotIn(label, resolved)
            resolved[label] = target
        return resolved

    def test_programs_datasets_and_detail_links(self):
        names = ["suite/a/first.fut:main", "suite/b/second.fut:other"]
        top, _ = self.profile(
            {
                name: {"datasets": {"small": result(), "large": result()}}
                for name in names
            }
        )
        programs = self.links(top / "index.html")
        self.assertEqual(set(programs), set(names))
        self.assertEqual(len(set(programs.values())), 2)
        for index in programs.values():
            datasets = self.links(index)
            self.assertEqual(set(datasets), {"small", "large"})
            for dataset in datasets.values():
                self.assertIn("Cost Centre Overview", self.links(dataset))

    def test_single_program_and_multiple_entry_points(self):
        for names in (
            ["prog.fut"],
            ["prog.fut:main", "prog.fut:other"],
            ["prefix.fut", "prefix-long.fut"],
            [str(self.root / "input/prog.fut") + ":main"],
        ):
            with self.subTest(names=names):
                top, _ = self.profile(
                    {name: {"datasets": {"input": result()}} for name in names}
                )
                programs = self.links(top / "index.html")
                self.assertEqual(set(programs), set(names))
                self.assertEqual(len(set(programs.values())), len(names))
                for index in programs.values():
                    self.assertNotEqual(index, top / "index.html")
                    self.assertEqual(set(self.links(index)), {"input"})

    def test_escaping_and_relative_paths(self):
        name = 'suite/a & <script> "é#?%.fut:main'
        dataset = 'input/ & <script> "é#?%'
        top, _ = self.profile({name: {"datasets": {dataset: result()}}})
        programs = self.links(top / "index.html")
        self.assertEqual(set(programs), {name})
        self.assertEqual(set(self.links(programs[name])), {dataset})

    def test_missing_and_failed_profiles(self):
        top, stderr = self.profile(
            {
                "good.fut:main": {
                    "datasets": {
                        "present": result(),
                        "missing": result(False),
                        "null": {**result(False), "profiling": None},
                        "failed": "execution failed",
                        "bad source": {
                            **result(),
                            "profiling": {
                                "memory": {},
                                "events": [
                                    {
                                        "name": "kernel",
                                        "duration": 10,
                                        "provenance": "missing.fut:1:1-2",
                                        "details": {},
                                    }
                                ],
                            },
                        },
                    }
                },
                "empty.fut:main": {"datasets": {"missing": result(False)}},
            }
        )
        programs = self.links(top / "index.html")
        self.assertEqual(set(programs), {"good.fut:main", "empty.fut:main"})
        self.assertEqual(
            set(self.links(programs["good.fut:main"])), {"present"}
        )
        self.assertEqual(self.links(programs["empty.fut:main"]), {})
        self.assertIn("no profiling information", stderr)
        self.assertIn("execution failed", stderr)
        self.assertIn("missing.fut", stderr)

    def test_empty_results(self):
        top, _ = self.profile({})
        self.assertEqual(self.links(top / "index.html"), {})


if __name__ == "__main__":
    unittest.main()
