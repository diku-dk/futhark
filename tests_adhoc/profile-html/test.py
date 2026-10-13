"""Test the real profiling CLI's HTML logs and timelines without a GPU."""

from __future__ import annotations

from html.parser import HTMLParser
import json
from pathlib import Path
import subprocess
import tempfile
import unittest
from urllib.parse import unquote, urlsplit


class Page(HTMLParser):
    def __init__(self, path: Path):
        super().__init__()
        self.tags: list[str] = []
        self.ids: list[str] = []
        self.links: list[str] = []
        self.blocks: dict[str, str] = {}
        self.block: str | None = None
        self.text = ""
        self.feed(path.read_text(encoding="utf-8"))

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]):
        self.tags.append(tag)
        attr = dict(attrs)
        if attr.get("id"):
            self.ids.append(str(attr["id"]))
        if tag == "a":
            self.links.append(str(attr["href"]))
        if tag == "pre":
            self.block = str(attr.get("id"))
            self.blocks[self.block] = ""

    def handle_endtag(self, tag: str):
        if tag == "pre":
            self.block = None

    def handle_data(self, data: str):
        self.text += data
        if self.block is not None:
            self.blocks[self.block] += data


def event(name="kernel", duration=12.5, provenance="unknown"):
    return {
        "name": name,
        "duration": duration,
        "provenance": provenance,
        "details": {"backend-specific": "retained in input JSON"},
    }


def result(events=None, log="execution log\n"):
    return {
        "runtimes": [100],
        "bytes": {"device": 256},
        "stderr": log,
        "profiling": {
            "memory": {"device": 256},
            "events": [event()] if events is None else events,
        },
    }


class ProfileHtmlTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name).resolve()

    def run_profile(self, payload, filename="results.json"):
        path = self.root / filename
        original = json.dumps(payload, ensure_ascii=False)
        path.write_text(original, encoding="utf-8")
        run = subprocess.run(
            ["futhark", "profile", str(path)],
            cwd=self.root,
            capture_output=True,
            text=True,
        )
        self.assertEqual(run.returncode, 0, run.stderr)
        self.assertEqual(path.read_text(encoding="utf-8"), original)
        return self.root / path.with_suffix(".prof").name, run.stderr

    def benchmark(self, datasets):
        return self.run_profile({"prog.fut:main": {"datasets": datasets}})

    def page(self, path):
        page = Page(path)
        self.assertNotIn("script", page.tags)
        self.assertNotIn("img", page.tags)
        self.assertIn("body", page.tags)
        self.assertIn("nav", page.tags)
        self.assertEqual(len(page.ids), len(set(page.ids)))
        for href in page.links:
            url = urlsplit(href)
            self.assertFalse(url.scheme or url.netloc or url.query)
            target = path.parent / unquote(url.path) if url.path else path
            self.assertTrue(target.is_file(), f"Broken link: {href}")
            if url.fragment:
                self.assertIn(unquote(url.fragment), Page(target).ids)
        return page

    def dataset(self, top, name):
        return self.page(top / "main" / (name + "-index.html"))

    def test_order_whitespace_unicode_and_escaping(self):
        log = '\n  allocation <script>alert("é")</script> & copy\t\nlast'
        events = [
            event('second <img src=x onerror="alert(1)"> & λ', 2.5),
            event("first", 0.0),
            event('second <img src=x onerror="alert(1)"> & λ', 1.25),
        ]
        top, _ = self.benchmark({"input": result(events, log)})
        page = self.dataset(top, "input")
        self.assertEqual(page.blocks["log-text"], log)
        self.assertEqual((top / "main/input.log").read_text(), log)
        timeline = (top / "main/input.timeline").read_text()
        self.assertEqual(page.blocks["timeline-text"], timeline)
        self.assertEqual(
            timeline,
            'second <img src=x onerror="alert(1)"> & λ\n'
            "Duration: 2.5 μs\nAt: unknown\n\n"
            "first\nDuration: 0.0 μs\nAt: unknown\n\n"
            'second <img src=x onerror="alert(1)"> & λ\n'
            "Duration: 1.25 μs\nAt: unknown\n",
        )
        self.assertIn("#log", page.links)
        self.assertIn("#timeline", page.links)
        self.assertTrue((top / "main/input.summary").is_file())
        self.assertTrue((top / "main/input.html/cost-centres.html").is_file())

    def test_missing_null_and_empty_fields(self):
        datasets = {
            "empty": result([], ""),
            "no-log": result(log=None),
            "log-only": result(log="no profile <&>"),
            "neither": {"runtimes": [], "bytes": {}},
            "failed": "execution failed",
        }
        datasets["log-only"]["profiling"] = None
        top, stderr = self.benchmark(datasets)
        empty = self.dataset(top, "empty")
        self.assertEqual(empty.blocks["log-text"], "")
        self.assertEqual(empty.blocks["timeline-text"], "")
        no_log = self.dataset(top, "no-log")
        self.assertNotIn("log-text", no_log.blocks)
        self.assertIn("No log recorded", no_log.text)
        log_only = self.dataset(top, "log-only")
        self.assertEqual(log_only.blocks["log-text"], "no profile <&>")
        self.assertNotIn("timeline-text", log_only.blocks)
        self.assertIn("No profiling information", log_only.text)
        self.assertFalse((top / "main/log-only.timeline").exists())
        self.assertFalse((top / "main/neither-index.html").exists())
        self.assertFalse((top / "main/failed-index.html").exists())
        self.assertIn("execution failed", stderr)
        self.assertIn("no profiling information", stderr)

    def test_unavailable_and_invalid_sources(self):
        for provenance in (
            "missing.fut:1:1-5",
            "<script>bad & provenance</script>",
            "invalid-utf8.fut:1:1-5",
        ):
            with self.subTest(provenance=provenance):
                (self.root / "invalid-utf8.fut").write_bytes(b"\xff")
                top, stderr = self.benchmark(
                    {"input": result([event(provenance=provenance)])}
                )
                page = self.dataset(top, "input")
                self.assertIn(provenance, page.blocks["timeline-text"])
                self.assertIn("Source information unavailable", page.text)
                self.assertEqual(page.blocks["log-text"], "execution log\n")
                self.assertTrue((top / "main/input.summary").is_file())
                self.assertTrue((top / "main/input.timeline").is_file())
                self.assertNotIn("cost-centres.html", " ".join(page.links))
                self.assertTrue(stderr)

    def test_source_heatmap_and_cost_centres(self):
        (self.root / "prog.fut").write_text("entry main x = x + 1\n")
        top, _ = self.benchmark(
            {"input": result([event(provenance="prog.fut:1:16-21")])}
        )
        page = self.dataset(top, "input")
        self.assertIn("Cost Centre Overview", page.text)
        self.assertTrue(
            any("cost-centres.html" in link for link in page.links)
        )
        self.assertTrue(any("prog.fut" in link for link in page.links))
        self.assertIn("At: prog.fut:1:16-21", page.blocks["timeline-text"])

    def test_raw_report(self):
        top, _ = self.run_profile(result()["profiling"], "raw.json")
        page = self.page(top / "index.html")
        self.assertEqual(
            page.blocks["timeline-text"], (top / "timeline").read_text()
        )
        self.assertNotIn("log-text", page.blocks)
        self.assertIn("No log recorded", page.text)
        self.assertTrue((top / "summary").is_file())
        self.assertFalse(list(top.glob("*.log")))

    def test_dataset_names_and_repeated_invocation(self):
        top, _ = self.benchmark({"input": result()})
        # Only the newly introduced in-page navigation is relevant here.
        page = Page(top / "main/input-index.html")
        for link in ("#log", "#timeline"):
            self.assertIn(link, page.links)
            self.assertIn(link[1:], page.ids)
        top, _ = self.benchmark({"input": result(log="stale")})
        top, _ = self.benchmark({"input": result(log=None)})
        page = self.dataset(top, "input")
        self.assertNotIn("log-text", page.blocks)
        self.assertNotIn("stale", page.text)
        self.assertFalse((top / "main/input.log").exists())


if __name__ == "__main__":
    unittest.main()
