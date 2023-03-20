#!/usr/bin/env python3

import sys
import argparse
import matplotlib.pyplot as plt
import matplotlib
import numpy as np
import json
import os
import pathlib
import textwrap
import re
import time
import random
import string
from matplotlib.ticker import FormatStrFormatter
from multiprocessing import Pool
from abc import ABC, abstractmethod
from typing import Any, Dict, Optional, List, Tuple, NamedTuple
from itertools import islice
from PIL import ImageFile
from collections import OrderedDict

assert sys.version_infoz >= (3, 9), "Use Python 3.9 or newer."
ImageFile.LOAD_TRUNCATED_IMAGES = True


class PlotJob(NamedTuple):
    program_path: str
    program: str
    dataset: str
    benchmark_result: Dict[str, Any]
    plots: Dict[str, str]


def mpe(runtimes: np.ndarray = None, **kwargs) -> str:
    """Computes the Mean percentage error and formats for printing."""
    factor = 100 / runtimes.shape[0]
    mpe = factor * ((runtimes - runtimes.mean()) / runtimes).sum()
    return f"{mpe:.2e}%"


def memory_usage(bytes: Dict[str, int] = None, **kwargs) -> str:
    """Computes the memory usages of devices and formats for printing."""

    def formatter(device, bs):
        return f"{format_bytes(bs)}@{device}"

    return ",".join(map(lambda a: formatter(*a)), bytes.items())


def confidence_interval(runtimes: np.ndarray = None, **kwargs) -> str:
    """Computes the 95% confidence interval and formats for printing."""

    mean = runtimes.mean()
    bound = 0.95 * runtimes.std(ddof=1) / np.sqrt(runtimes.shape[0])
    return f"[{format_time(mean - bound)}; {format_time(mean + bound)}]"


# Here other descriptors can be added.
DESCRIPTORS = {
    "Mean Percentage Error": mpe,
    "Memory Usage": memory_usage,
    "95% Confidence Interval": confidence_interval,
}


def random_string(size: int) -> str:
    """Creates a random alphanumeric string of a given size."""
    letters = string.ascii_letters + string.digits
    return "".join(random.choice(letters) for _ in range(size))


def format_bytes(x: int) -> str:
    """Tries to find a suitable unit for input x given in bytes."""

    units = [
        ("TiB", 1 / (1024**4)),
        ("GiB", 1 / (1024**3)),
        ("MiB", 1 / (1024**2)),
        ("KiB", 1 / 1024),
        ("bytes", 1),
    ]

    for unit, factor in units:
        temp = factor * x
        if temp > 1:
            return f"{temp:.2f}{unit}"

    return f"{x * units[-1][1]:.2f}{units[-1][0]}"


def format_time(x: int) -> str:
    """Tries to find a suitable time unit for input x."""

    units = [
        ("h", 1 / (60 * 60 * 10**6)),
        ("min", 1 / (60 * 10**6)),
        ("s", 10 ** (-6)),
        ("ms", 10 ** (-3)),
        ("µs", 1),
    ]

    for unit, factor in units:
        temp = factor * x
        if temp > 1:
            return f"{temp:.2f}{unit}"

    return f"{x * units[-1][1]:.2f}{units[-1][0]}"


class PlotType(ABC):
    @abstractmethod
    def plot(self, ax, **kwargs) -> None:
        """Method used to create a plot."""
        raise NotImplementedError()

    @classmethod
    @abstractmethod
    def name(cls) -> str:
        """The name of the plot."""
        raise NotImplementedError()

    def __eq__(self, other) -> bool:
        return self.name() == other.name()

    def __lt__(self, other) -> bool:
        return self.name() < other.name()

    @classmethod
    def find_str_formatter(cls, z) -> Tuple[FormatStrFormatter, float]:
        """
        Tries to find a suitable time unit for the given numpy array. The
        scaling factor is returned and a string formatter for matplotlib axis
        using the suitable unit is returned.
        """
        units = [
            (FormatStrFormatter("$%.2fh$"), 1 / (60 * 60 * 10**6)),
            (FormatStrFormatter("$%.2fmin$"), 1 / (60 * 10**6)),
            (FormatStrFormatter("$%.2fs$"), 10 ** (-6)),
            (FormatStrFormatter("$%.2fms$"), 10 ** (-3)),
            (FormatStrFormatter("$%.2f\\mu s$"), 1.0),
        ]

        for unit, factor in units:
            if factor * z.max() > 1:
                return unit, factor

        return units[-1][0], units[-1][1]


class PerRun(PlotType):
    """Create a plot with runtime vs iteration number as plot."""

    def plot(self, ax, runtimes=None, **kwargs) -> None:
        ax.set_title("Per Runtime")
        x = np.arange(len(runtimes))
        y = runtimes
        formatter, factor = PerRun.find_str_formatter(y)
        y = y * factor
        runtimes = ax.scatter(x, y, marker=".")
        ax.legend([runtimes], ["Runtimes"])
        ax.xaxis.set_tick_params(rotation=45)
        ax.yaxis.set_major_formatter(formatter)
        ax.set_ylabel("Runtime")
        ax.set_xlabel("$i$th Runtime")
        ax.grid()

    @classmethod
    def name(cls) -> str:
        return "per_run"


class CumsumPerRun(PlotType):
    """Create a plot with cumulative runtime vs iteration number as plot."""

    def plot(self, ax, runtimes=None, **kwargs) -> None:
        ax.set_title("Cumulative runtime")
        x = np.arange(len(runtimes))
        y = np.cumsum(runtimes)
        formatter, factor = CumsumPerRun.find_str_formatter(y)
        y = y * factor
        X = np.vstack([x, np.ones(len(x))]).T
        slope, intercept = np.linalg.lstsq(X, y, rcond=None)[0]
        ax.scatter(x, y, marker=".")
        ax.xaxis.set_tick_params(rotation=45)
        ax.plot(x, slope * x + intercept, color="black")
        ax.set_ylabel("Cumulative Runtime")
        ax.set_xlabel("$i$th Runtime")
        ax.yaxis.set_major_formatter(formatter)
        ax.grid()

    @classmethod
    def name(cls) -> str:
        return "cumsum_per_run"


class RuntimeDensities(PlotType):
    """Creates a plots the probability density of the runtimes."""

    def plot(self, ax, runtimes=None, **kwargs) -> None:
        ax.set_title("Runtime Densities")
        bincount = np.trim_zeros(np.bincount(runtimes))
        y = bincount / len(runtimes)
        x = np.arange(runtimes.min(), runtimes.max() + 1)
        formatter, factor = RuntimeDensities.find_str_formatter(x)
        x = x * factor
        ax.xaxis.set_tick_params(rotation=45)
        ax.xaxis.set_major_formatter(formatter)
        mean = ax.axvline(x=runtimes.mean() * factor, color="k", label="mean")
        ymin = y.min()
        ymax = y.max()
        padding = abs(ymax - ymin) * 0.05
        ax.set_ylim(ymin - padding, ymax + padding)
        ax.legend([mean], ["Mean Runtime"])
        ax.set_xlabel("Runtime")
        ax.set_ylabel("Density")
        ax.plot(x, y, linestyle="-")
        ax.grid()

    @classmethod
    def name(cls) -> str:
        return "runtime_densities"


class LagPlot(PlotType):
    """
    Creates a lag plot where given some runtimes it copies the array and
    shifts them by one and then plots the two data points vs each other.
    """

    def plot(self, ax, runtimes=None, **kwargs) -> None:
        ax.set_title("Lag Plot")
        x = runtimes
        formatter, factor = LagPlot.find_str_formatter(x)
        x = x * factor
        y = np.roll(x, 1)
        ax.yaxis.set_major_formatter(formatter)
        ax.xaxis.set_major_formatter(formatter)
        ax.xaxis.set_tick_params(rotation=45)
        ax.set_xlabel("The $i$th Runtime")
        ax.set_ylabel("The $i$th + 1 Runtime")
        ax.scatter(x, y, marker=".")
        ax.grid()

    @classmethod
    def name(cls) -> str:
        return "lag_plot"


ALL_PLOT_TYPES = OrderedDict(
    {plot_type.name(): plot_type for plot_type in PlotType.__subclasses__()}
)


class Plotter:
    """Class that will plot and save many figures on a process."""

    def __init__(
        self,
        plot_types: List[PlotType],
        dpi: Any = "200",
        transparent: bool = False,
    ) -> None:
        self.dpi = dpi
        self.plot_types = list(sorted(plot_types))
        self.fig, self.ax = plt.subplots(figsize=(6.4, 5.8))
        self.transparent = transparent
        self.backends = {
            ".png": "AGG",
            ".pdf": "PDF",
            ".ps": "PS",
            ".eps": "PS",
            ".svg": "SVG",
            ".pgf": "PGF",
        }

    def plot(self, data: Dict[str, PlotJob]) -> None:
        """
        Will use the plotter function on all the given data. The data is a
        dictionary where the key is the destination and the value is the values
        that will be passed to the plotter function.
        """

        for datapoint in data.values():
            plots = datapoint.plots
            for plotter in self.plot_types:
                plotter.plot(self.ax, **datapoint.benchmark_result)

                program = datapoint.program
                dataset = datapoint.dataset
                dest = plots[plotter.name()]

                suptitle = textwrap.shorten(rf"{program}: {dataset}", 50)
                self.fig.suptitle(suptitle)
                self.fig.tight_layout()
                # For some reason a syntax error may occour but the savefig
                # function will for some reason work afterwards.
                for _ in range(10):
                    try:
                        ext = pathlib.Path(dest).suffix
                        self.fig.savefig(
                            dest,
                            bbox_inches="tight",
                            dpi=self.dpi,
                            backend=self.backends[ext],
                            transparent=self.transparent,
                        )
                        print(f"Done creating: {dest}")
                        break
                    except SyntaxError:
                        time.sleep(1)
                else:
                    print(
                        (
                            f'Figure "{dest}" did not get saved this is most '
                            "likely an internal error. try specifying the "
                            "specific program with --program."
                        )
                    )
                self.ax.cla()
        plt.close(self.fig)


def chunks(data: Dict, size: int):
    """Generator that makes sub-dictionaries of a maximum size."""
    it = iter(data)
    for _ in range(0, len(data), size):
        yield {k: data[k] for k in islice(it, size)}


def get_args() -> Any:
    """Gets the arguments used in the program."""
    parser = argparse.ArgumentParser(
        prog="Futhark Plots", description="Makes plots for futhark benchmarks."
    )
    parser.add_argument(
        "filename",
        metavar="FILE",
        help=(
            "the benchmark results as a json file generated by futhark "
            "bench."
        ),
    )
    parser.add_argument(
        "--programs",
        metavar="PROGRAM0,PROGRAM1,...",
        help=(
            "the specific programs the plots will be generated from. Default"
            "is all programs."
        ),
    )
    parser.add_argument(
        "--plots",
        metavar="PLOTTYPE0,PLOTTYPE1,...",
        help=(
            f"the type of plots that will be generated which can be "
            f'{", ".join(ALL_PLOT_TYPES.keys())}. Default is all plots.'
        ),
    )
    parser.add_argument(
        "--filetype",
        default="png",
        metavar="BACKEND",
        help=(
            "the file type used, these can be found on the matplotlib "
            "website."
        ),
    )
    parser.add_argument(
        "--transparent",
        action="store_true",
        help="flag to use if the bagground should be transparent.",
    )

    return parser.parse_args()


def format_arg_list(args: Optional[str]) -> Optional[str]:
    """
    Takes a string of form 'a, b, c, d' and makes a list ['a', 'b', 'c', 'd']
    """
    if args is None:
        return None

    return set(map(lambda arg: arg.strip(), args.split(",")))


def make_plot_jobs_and_directories(
    programs: List[str],
    data: Dict[str, Dict[str, Dict[str, Any]]],
    file_type: str,
    plot_types: List[str],
    root: str = "graphs",
) -> Tuple[Dict[str, PlotJob], Dict[str, List[str]]]:
    """Makes dictionary with plot jobs where plot_jobs are the jobs."""

    plot_jobs = dict()
    folder_content = dict()

    def remove_characters(characters, text):
        rep = {re.escape(k): "" for k in characters}
        pattern = re.compile("|".join(rep.keys()))
        return pattern.sub(lambda m: rep[re.escape(m.group(0))], text)

    for program_path in programs:
        datasets = data.get(program_path).get("datasets")
        program_name = pathlib.Path(program_path).name
        program_directory = os.path.dirname(program_path)
        for dataset_path, dataset_dict in datasets.items():
            dataset_name = pathlib.Path(dataset_path).name
            bad_chars = [" ", "#", '"', "/"]
            dataset_path = remove_characters(bad_chars, dataset_path)
            dataset_name_striped = dataset_path.replace(".in", "")
            raw_filename = f"{program_name}_{dataset_name_striped}"
            dataset_filename = raw_filename[:100].replace(" ", "_")
            directory = os.path.join(
                root, program_directory, pathlib.Path(program_path).name
            )
            directory = "." if directory == "" else directory
            plot_file_name = os.path.join(
                directory, f"{dataset_filename}_{random_string(16)}"
            )
            benchmark_result = dataset_dict.copy()
            np_runtimes = np.array(benchmark_result.get("runtimes"))
            benchmark_result["runtimes"] = np_runtimes

            os.makedirs(directory, exist_ok=True)

            if folder_content.get(directory) is None:
                folder_content[directory] = []

            folder_content[directory].insert(0, plot_file_name)

            plot_jobs[plot_file_name] = PlotJob(
                program_path,
                program_name,
                dataset_name,
                benchmark_result,
                {
                    plot_type: f"{plot_file_name}_{plot_type}.{file_type}"
                    for plot_type in plot_types
                },
            )

    return plot_jobs, folder_content


def make_html_descriptors(plot_job: PlotJob) -> str:
    """Makes a table with statistical descriptors for the plot_job"""

    def row(name, func):
        result = func(**plot_job.benchmark_result)
        return rf"<tr><td>{name}:</td><td>{result}</td></tr>"

    return f"""<table>
    <tbody>
        {''.join(map(lambda a: row(*a), DESCRIPTORS.items()))}
    </tbody>
</table>"""


def make_html(
    folder_content: Dict[str, List[str]],
    plot_jobs: Dict[str, PlotJob],
    root: str,
) -> str:
    """Makes a simpel html document with links to each section with plots."""

    def make_key(s, size):
        return f'{"".join(e for e in s if e.isalnum())}{random_string(size)}'

    plot_jobs_keys = dict()
    for key in plot_jobs.keys():
        program = plot_jobs[key].program
        dataset = plot_jobs[key].dataset
        plot_jobs_keys[key] = make_key(program + dataset, 32)

    folder_keys = {folder: make_key(folder, 32) for folder in folder_content}

    root_prefix = f"{root}/"

    def make_li(p: str) -> str:
        """
        Makes a single bullet point for a given benchmark's dataset.
        """
        dataset = plot_jobs[p].dataset
        key = plot_jobs_keys[p]
        return rf"<li><a href=#{key}>{dataset}</a></li>"

    def make_list(path: str) -> str:
        """
        Creates the list which shows the structure of benchmarks and links to
        the sections.
        """
        sorted_paths = sorted(os.listdir(path))
        directory = list(map(lambda p: os.path.join(path, p), sorted_paths))

        isdir = os.path.isdir

        if len(directory) == 1 and isdir(directory[0]) and path == root:
            return make_list(directory[0])

        before = "".join(map(make_li, folder_content.get(path, [])))
        lis = "".join(map(make_list, directory))
        pretty_path = path.removeprefix(root_prefix)

        if folder_keys.get(path) is not None:
            pretty_path = f"<a href=#{folder_keys.get(path)}>{pretty_path}</a>"

        return rf"<li>{pretty_path}</li><ul>{before}{lis}</ul>"

    def make_subsection(plot_file: str, plot_job: PlotJob) -> str:
        """
        Makes a subsection with plots and statistical descriptors.
        """
        dataset = plot_job.dataset
        program = plot_job.program
        key = plot_jobs_keys[plot_file]
        descriptors = make_html_descriptors(plot_job)

        plots = "".join(
            map(lambda plot: f"<img src='{plot}'/>", plot_job.plots.values())
        )
        header = rf"<h3><a href=#{key}>{program}: {dataset}</a></h3>"
        return rf"<section id={key}>{header}{plots}{descriptors}</section>"

    def make_section(folder: str, dataset_plot_files: List[str]) -> str:
        """
        Makes a section with all the plots and descriptors for a given
        benchmark's datasets.
        """
        get = plot_jobs.get
        sub_data = map(lambda a: (a, get(a)), dataset_plot_files)
        subsections = "".join(map(lambda a: make_subsection(*a), sub_data))
        pretty_folder = folder.removeprefix(root_prefix)
        folder_key = folder_keys[folder]
        header = rf"<h2><a href=#{folder_key}>{pretty_folder}</a></h2>"
        return rf"<section id={folder_key}>{header}{subsections}</section>"

    width = 100 // len(next(iter(plot_jobs.values())).plots)
    lis = make_list(root)
    sorted_content = sorted(folder_content.items())
    sections = "".join(map(lambda a: make_section(*a), sorted_content))
    return f"""<head>
    <style type="text/css">
        img {{
            width: {width}%
        }}
        body {{
            font-family: sans-serif;
            padding: 0px;
            margin: 0px;
            margin-left: auto;
            margin-right: auto;
            overflow-y: scroll;
            line-height: 1.7;
        }}
        section {{
            border: 2px solid transparent;
        }}
        section:target {{
            border: 2px solid black;
        }}
    </style>
    <nav>
        <ul>
            {lis}
        </ul>
    </nav>
    {sections}
</head>"""


def task(plot_jobs: Dict[str, PlotJob]) -> None:
    """Begins plotting, it is used"""
    global plots
    plot_types = [
        plot_type()
        for key, plot_type in ALL_PLOT_TYPES.items()
        if key in plot_types_used
    ]
    plotter = Plotter(plot_types, dpi=200, transparent=transparent)
    plotter.plot(plot_jobs)


def main() -> None:
    global plot_types_used
    global transparent
    plt.rcParams.update(
        {
            "ytick.color": "black",
            "xtick.color": "black",
            "axes.labelcolor": "black",
            "axes.edgecolor": "black",
            "axes.axisbelow": True,
            "text.usetex": False,
            "axes.prop_cycle": matplotlib.cycler(color=["#5f021f"]),
        }
    )

    args = get_args()
    filename = pathlib.Path(args.filename).stem
    data = json.load(open(args.filename, "r"))
    programs = format_arg_list(args.programs)
    plot_types_used = format_arg_list(args.plots)
    filetype = args.filetype
    transparent = args.transparent

    root = f"{filename}-plots"

    if os.path.exists(root):
        raise Exception(
            (
                f'The folder "{root}" must be removed before the plots can be '
                "made."
            )
        )

    if plot_types_used is None:
        plot_types_used = list(sorted(ALL_PLOT_TYPES.keys()))
    else:
        plot_types_used = list(sorted(plot_types_used))
        existing_plot_types = list(ALL_PLOT_TYPES.keys())
        for plot_type in plot_types_used:
            if plot_type not in existing_plot_types:
                existing_plot_types = ", ".join(existing_plot_types)
                raise Exception(
                    (
                        '"{plot_type}" is not a plot type try '
                        f"{existing_plot_types}"
                    )
                )

    if programs is None:
        programs = set(data.keys())
    else:
        programs = set(programs)
        keys = set(data.keys())
        if not programs.issubset(keys):
            diff = ", ".join(programs.difference(keys))
            raise Exception(f'"{diff}" are not valid keys.')

    plot_jobs, folder_content = make_plot_jobs_and_directories(
        programs, data, filetype, plot_types_used, root=root
    )

    with open(f"{filename}.html", "w") as fp:
        fp.write(make_html(folder_content, plot_jobs, root))

    with Pool(16) as p:
        p.map(task, chunks(plot_jobs, max(len(plot_jobs) // 32, 1)))


if __name__ == "__main__":
    main()
