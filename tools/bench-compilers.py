#!/usr/bin/env python3
#
# Construct PNG graphs of compile- and run-time performance for
# historical versions of the Futhark compiler.  The legend is almost
# useless, so manual investigation is needed to get something usable
# out of this.
#
# This program is quite naive in its construction of commands passed
# to os.system, so don't run it from a directory with interesting
# characters in its absolute path.  Run it from an initially empty
# directory, because it will create subdirectories for its own
# purposes.
#
# This program has lots of hacks and special cases to handle the
# various changes in Futhark tooling over the years.  But all things
# considered, it's not too bad.
#
# Regarding the special 'nightly' version, delete it from the
# releases/ directory if you want this script to download a newer one.
#
# $ bench-compilers.py plot_runtime 0.1.0 0.2.0 0.3.0 0.4.0 0.4.1 0.5.1 0.6.1 0.6.2 0.6.3 0.7.1 0.7.2 0.7.3 0.7.4 0.8.1 0.1 0.1 0.10.2 0.11.1 0.11.2 0.12.1 0.12.2 0.12.3 0.13.1 0.13.2 0.14.1 0.15.1 0.15.2 0.15.3 0.15.4 0.15.5 0.15.6 0.15.7 0.15.8 0.16.1 0.16.2 0.16.3 0.16.4 0.16.5 0.17.2 0.17 .3 0.18.1 0.18.2 0.18.3 0.18.4 0.18.5 0.18.6 0.19.1 0.19.2 0.19.3 0.19.4 0.19.5 nightly # nopep8
#
# The example above skips 0.9.1 because a few benchmarks do not
# validate with it.  Seems like there was a bug with certain
# reductions.


import sys
import os.path
import urllib.request
import subprocess
from pathlib import Path
import json
import time
import matplotlib.pyplot as plt
import matplotlib.ticker
import numpy as np

RELEASES_DIR = "releases"
GITS_DIR = "gits"
JSON_DIR = "json"
FUTHARK_BACKEND = "opencl"
FUTHARK_REPO = "https://github.com/diku-dk/futhark.git"
FUTHARK_BENCHMARKS_REPO = "https://github.com/diku-dk/futhark-benchmarks.git"
FUTHARK_DIR = os.path.join(GITS_DIR, "futhark")
FUTHARK_BENCHMARKS_DIR = os.path.join(GITS_DIR, "futhark-benchmarks")
BENCH_RUNS = 20

# For newer versions than this, the 'futhark-benchmarks' submodule
# points to the right revision already.
versions_to_benchmark_commits = {
    "0.3.1": "3b9c6cd06784fbf94c40a30f4302eeef119352b7",
    "0.3.0": "289000e5734705b45840ee6315bd683ab14b6ddb",
    "0.2.0": "33aabdd88bb3c935934032c5d49a7563f7078322",
    "0.1.0": "24cba07b5b70d3cbcf4195108a4caddbb649481f",
}

# Hand-curated set of benchmarks and corresponding data sets that have
# existed since version 0.1.0
benchmarks = {
    "futhark-benchmarks/accelerate/canny/canny.fut": "data/lena512.in",
    "futhark-benchmarks/accelerate/crystal/crystal.fut": '#6 ("4000i32 30.0f32 50i32 1i32 1.0f32")',
    "futhark-benchmarks/accelerate/fluid/fluid.fut": "benchmarking/medium.in",
    "futhark-benchmarks/accelerate/mandelbrot/mandelbrot.fut": '#5 ("8000i32 8000i32 -0.7f32 0.0f32 3.067f32 100i32 16....")',
    "futhark-benchmarks/accelerate/nbody/nbody.fut": "data/100000-bodies.in",
    "futhark-benchmarks/accelerate/tunnel/tunnel.fut": '#5 ("10.0f32 8000i32 8000i32")',
    "futhark-benchmarks/finpar/LocVolCalib.fut": "LocVolCalib-data/large.in",
    "futhark-benchmarks/finpar/OptionPricing.fut": "OptionPricing-data/large.in",
    "futhark-benchmarks/jgf/crypt/crypt.fut": "crypt-data/medium.in",
    "futhark-benchmarks/jgf/series/series.fut": "data/1000000.in",
    "futhark-benchmarks/misc/heston/heston32.fut": "data/100000_quotes.in",
    "futhark-benchmarks/misc/heston/heston64.fut": "data/100000_quotes.in",
    "futhark-benchmarks/misc/radix_sort/radix_sort_large.fut": "data/radix_sort_1M.in",
    "futhark-benchmarks/parboil/mri-q/mri-q.fut": "data/large.in",
    "futhark-benchmarks/parboil/sgemm/sgemm.fut": "data/medium.in",
    "futhark-benchmarks/parboil/stencil/stencil.fut": "data/default.in",
    "futhark-benchmarks/parboil/tpacf/tpacf.fut": "data/large.in",
    "futhark-benchmarks/rodinia/backprop/backprop.fut": "data/medium.in",
    "futhark-benchmarks/rodinia/bfs/bfs_filt_padded_fused.fut": "data/graph1MW_6.in",
    "futhark-benchmarks/rodinia/cfd/cfd.fut": "data/fvcorr.domn.193K.toa",
    "futhark-benchmarks/rodinia/hotspot/hotspot.fut": "data/1024.in",
    "futhark-benchmarks/rodinia/kmeans/kmeans.fut": "data/kdd_cup.in",
    "futhark-benchmarks/rodinia/lavaMD/lavaMD.fut": "data/10_boxes.in",
    "futhark-benchmarks/rodinia/lud/lud.fut": "data/2048.in",
    "futhark-benchmarks/rodinia/myocyte/myocyte.fut": "data/medium.in",
    "futhark-benchmarks/rodinia/nn/nn.fut": "data/medium.in",
    "futhark-benchmarks/rodinia/pathfinder/pathfinder.fut": "data/medium.in",
    "futhark-benchmarks/rodinia/srad/srad.fut": "data/image.in",
}


# Some data sets changed names over time - renormalise here.
def datamap(d):
    m = {
        '#6 ("4000i32 30.0f32 50i32 1i32 1.0f32")': '#5 ("4000i32 30.0f32 50i32 1i32 1.0f32")',
        '#5 ("8000i32 8000i32 -0.7f32 0.0f32 3.067f32 100i32 16....")': '#4 ("8000i32 8000i32 -0.7f32 0.0f32 3.067f32 100i32 16....")',
        '#5 ("10.0f32 8000i32 8000i32")': '#4 ("10.0f32 8000i32 8000i32")',
    }
    if d in m:
        return m[d]
    else:
        return d


def shell(cmd):
    print("$ {}".format(cmd))
    if os.system(cmd) != 0:
        raise Exception("shell command failed")


def version_before(x, y):
    if x == "nightly":
        return False
    else:
        return tuple(map(int, x.split("."))) < tuple(map(int, y.split(".")))


def runtime_json_for_version(version):
    return os.path.join(JSON_DIR, "{}.json".format(version))


def compile_json_for_version(version):
    return os.path.join(JSON_DIR, "{}-compile.json".format(version))


def release_for_version(version):
    return os.path.join(
        RELEASES_DIR, "futhark-{}-linux-x86_64".format(version)
    )


def tarball_for_version(version):
    return os.path.join(
        RELEASES_DIR, "futhark-{}-linux-x86_64.tar.xz".format(version)
    )


def futhark_compile_for_version(version):
    if version_before(version, "0.9.1"):
        return os.path.abspath(
            os.path.join(
                release_for_version(version),
                "bin",
                "futhark-{}".format(FUTHARK_BACKEND),
            )
        )
    else:
        return os.path.join(
            release_for_version(version),
            "bin/futhark {}".format(FUTHARK_BACKEND),
        )


def futhark_bench_for_version(version):
    bs = " ".join(list(benchmarks.keys()))
    if version_before(version, "0.9.1"):
        prog = os.path.abspath(
            os.path.join(release_for_version(version), "bin", "futhark-bench")
        )
        compiler = os.path.abspath(
            os.path.join(
                release_for_version(version),
                "bin",
                "futhark-{}".format(FUTHARK_BACKEND),
            )
        )
        return "{} -r {} --compiler={} {}".format(
            prog, BENCH_RUNS, compiler, bs
        )
    else:
        prog = os.path.abspath(
            os.path.join(release_for_version(version), "bin", "futhark")
        )
        return "{} bench -r {} --backend={} --futhark={} {}".format(
            prog, BENCH_RUNS, FUTHARK_BACKEND, prog, bs
        )


def tarball_url(version):
    return "https://futhark-lang.org/releases/futhark-{}-linux-x86_64.tar.xz".format(
        version
    )


def ensure_tarball(version):
    f = tarball_for_version(version)
    if not os.path.exists(f):
        print("Downloading {}...".format(tarball_url(version)))
        urllib.request.urlretrieve(tarball_url(version), f)


def ensure_release(version):
    if not os.path.exists(release_for_version(version)):
        ensure_tarball(version)
        print("Extracting {}...".format(tarball_for_version(version)))
        shell(
            "cd {} && tar xf ../{}".format(
                RELEASES_DIR, tarball_for_version(version)
            )
        )


def ensure_repo(what, url):
    dir = os.path.join(GITS_DIR, what)
    if os.path.exists(dir):
        shell("cd {} && git checkout master && git pull".format(dir))
    else:
        shell("git clone --recursive {} {}".format(url, dir))
    return dir


def ensure_futhark_repo():
    return ensure_repo("futhark", FUTHARK_REPO)


def ensure_benchmarks(version):
    if version in versions_to_benchmark_commits:
        ensure_repo("futhark-benchmarks", FUTHARK_BENCHMARKS_REPO)
        shell(
            "cd {} && git checkout -f {}".format(
                FUTHARK_BENCHMARKS_DIR, versions_to_benchmark_commits[version]
            )
        )
        return GITS_DIR
    else:
        d = ensure_futhark_repo()
        tag = "master" if version == "nightly" else "v{}".format(version)
        shell(
            "cd {} && git checkout -f {} && git submodule update".format(
                d, tag
            )
        )
        return os.path.join(d)


def produce_runtime_json(version):
    f = runtime_json_for_version(version)
    ensure_release(version)
    ensure_repo("futhark", FUTHARK_REPO)
    d = ensure_benchmarks(version)
    json_f = os.path.abspath(runtime_json_for_version(version))
    shell(
        "cd {} && {} --json={}".format(
            d, futhark_bench_for_version(version), json_f
        )
    )


def ensure_runtime_json(version):
    f = runtime_json_for_version(version)
    if not os.path.exists(f):
        produce_runtime_json(version)
    return f


def produce_compile_json(version):
    ensure_release(version)
    d = ensure_benchmarks(version)
    compiletimes = {}
    for b in benchmarks:
        bef = time.time()
        shell(
            "{} {}".format(
                futhark_compile_for_version(version), os.path.join(d, b)
            )
        )
        aft = time.time()
        compiletimes[b] = aft - bef
    with open(compile_json_for_version(version), "w") as f:
        json.dump(compiletimes, f)


def ensure_compile_json(version):
    f = compile_json_for_version(version)
    if not os.path.exists(f):
        produce_compile_json(version)
    return f


def ensure_json(version):
    ensure_runtime_json(version)
    ensure_compile_json(version)


def load_json(f):
    return json.load(open(f, "r"))


def plot_runtime(versions):
    fastest = {}
    results_per_bench = {}
    for v in versions:
        results = load_json(ensure_runtime_json(v))
        for b, d in benchmarks.items():
            try:
                x = np.median(results[b]["datasets"][d]["runtimes"])
            except KeyError:
                x = np.median(results[b]["datasets"][datamap(d)]["runtimes"])

            if (b, d) in fastest:
                fastest[b, d] = min(fastest[b, d], x)
            else:
                fastest[b, d] = x

            if (b, d) in results_per_bench:
                results_per_bench[b, d] += [x]
            else:
                results_per_bench[b, d] = [x]

    for b, d in benchmarks.items():
        results_per_bench[b, d] = (
            np.array(results_per_bench[b, d]) / fastest[b, d]
        )

    fig, ax = plt.subplots()

    ax.set_yscale("log")
    ax.yaxis.set_major_formatter(matplotlib.ticker.ScalarFormatter())
    ax.set_ylabel("Runtime relative to fastest")
    plt.xticks(rotation=90)

    for b, d in benchmarks.items():
        xs = versions + ["..."]
        ys = np.append(results_per_bench[b, d], [results_per_bench[b, d][-1]])
        ax.plot(xs, ys, label=b[len("futhark-benchmarks/") :])

    ax.grid()

    ax.legend(
        bbox_to_anchor=(0, -0.35), ncol=3, loc="center left", fontsize=5.5
    )
    fig.savefig("runtime.png", dpi=300, figsize=(12, 6), bbox_inches="tight")


def plot_compiletime(versions):
    fastest = {}
    results_per_bench = {}
    for v in versions:
        results = load_json(ensure_compile_json(v))
        for b in benchmarks:
            x = np.mean(results[b])

            if b in fastest:
                fastest[b] = min(fastest[b], x)
            else:
                fastest[b] = x

            if b in results_per_bench:
                results_per_bench[b] += [x]
            else:
                results_per_bench[b] = [x]

    for b in benchmarks:
        results_per_bench[b] = np.array(results_per_bench[b]) / fastest[b]

    fig, ax = plt.subplots()

    ax.set_yscale("log")
    ax.yaxis.set_major_formatter(matplotlib.ticker.ScalarFormatter())
    ax.set_ylabel("Compilation time relative to fastest")
    plt.xticks(rotation=90)

    for b in benchmarks:
        xs = versions + ["..."]
        ys = np.append(results_per_bench[b], [results_per_bench[b][-1]])
        ax.plot(xs, ys, label=b[len("futhark-benchmarks/") :])

    ax.grid()

    ax.legend(
        bbox_to_anchor=(0, -0.35), ncol=3, loc="center left", fontsize=5.5
    )
    fig.savefig(
        "compiletime.png", dpi=300, figsize=(12, 6), bbox_inches="tight"
    )


if __name__ == "__main__":
    _, cmd, *args = sys.argv

    Path(RELEASES_DIR).mkdir(parents=True, exist_ok=True)
    Path(GITS_DIR).mkdir(parents=True, exist_ok=True)
    Path(JSON_DIR).mkdir(parents=True, exist_ok=True)

    if cmd == "bench":
        ensure_json(*args)
    elif cmd == "benchdir":
        print(os.path.join(ensure_benchmarks(*args), "futhark-benchmarks"))
    elif cmd == "plot_runtime":
        plot_runtime(args)
    elif cmd == "plot_compiletime":
        plot_compiletime(args)
    else:
        print("Invalid command: {}\n".format(cmd))
        sys.exit(1)
