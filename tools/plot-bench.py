import argparse
import matplotlib.pyplot as plt
import matplotlib
import numpy as np
import json
import io
import os
import pathlib
import textwrap
import re
import time
from matplotlib.ticker import FormatStrFormatter
from multiprocessing import Process
from abc import ABC, abstractmethod
from typing import Union, Any, Dict, Optional, Set
from itertools import islice
from PIL import ImageFile
ImageFile.LOAD_TRUNCATED_IMAGES = True


def escape_latex(text: str) -> str:
    """
    Replaces not escaped characters with escaped LaTeX strings.
    """
    rep = {
        r'&': r'\&',
        r'%': r'\%',
        r'$': r'\$',
        r'#': r'\#',
        r'{': r'\{',
        r'}': r'\}'
    }
    rep = {re.escape(k): v for k, v in rep.items()}
    pattern = re.compile("|".join(rep.keys()))
    return pattern.sub(lambda m: rep[re.escape(m.group(0))], text)


class Plotter(ABC):
    """Class that will plot and save many figures on a process."""

    def __init__(self, dpi: Any = 'figure', transparent: bool = False) -> None:
        self.dpi = dpi
        self.fig, self.ax = plt.subplots()
        self.transparent = transparent
        self.backends = {
            '.png': 'AGG',
            '.pdf': 'PDF',
            '.ps': 'PS',
            '.eps': 'PS',
            '.svg': 'SVG',
            '.pgf': 'PGF'
        }
    
    def _default_title(self, program: str = None, dataset: str = None):
        """Sets the title for the current axes."""
        if dataset is None or program is None:
            return

        self.ax.set_title(escape_latex(textwrap.shorten(fr'{program}: {dataset}', 50)))
    
    def _task(self, data: Dict[Union[io.BufferedWriter, str], Any]):
        """
        Will use the plotter function on all the given data. The data is a
        dictionary where the key is the destination and the value is the values
        that will be passed to the plotter function.
        """
        for dest, datapoint in data.items():
            self.plotter(**datapoint)
            file_extension = pathlib.Path(dest).suffix
            
            # For some reason a syntax error may occour but the savefig function
            # will for some reason work afterwards.
            for _ in range(10):
                try:
                    self.fig.savefig(
                        dest,
                        bbox_inches='tight',
                        dpi=self.dpi,
                        backend=self.backends[file_extension],
                        transparent=self.transparent
                    )
                    break
                except SyntaxError:
                    time.sleep(1)
            else:
                print((f'Figure "{dest}" did not get saved this is most likely an internal error.'
                       'try specifying the specific program with --program.'))
            
            self.ax.cla()
        plt.close(self.fig)

    def plot(self, data: Dict[Union[io.BufferedWriter, str], Any]):
        """Will start a thread that makes all the plots from the data."""
        
        p = Process(target=self._task, args=(data,))
        p.start()
        return p

    @abstractmethod
    def plotter(self, **kwargs):
        """Method used to create a plot."""
        raise NotImplementedError()


class PerRun(Plotter):
    """Create a plot with runtime vs iteration number as plot."""

    def plotter(self, runtimes=None, **kwargs):
        self._default_title(**kwargs)
        x = np.arange(len(runtimes))
        y = runtimes
        runtimes = self.ax.scatter(x, y, marker='.')
        self.ax.legend([runtimes], ['Runtimes'])
        self.ax.yaxis.set_major_formatter(FormatStrFormatter('$%d\\mu s$'))
        self.ax.set_ylabel('Runtime')
        self.ax.set_xlabel('Iteration Number')
        self.ax.grid()


class CumsumPerRun(Plotter):
    """Create a plot with cumulative runtime vs iteration number as plot."""
    
    def plotter(self, runtimes=None, **kwargs):
        self._default_title(**kwargs)
        x = np.arange(len(runtimes))
        y = np.cumsum(runtimes) / 1000
        X = np.vstack([x, np.ones(len(x))]).T
        slope, intercept = np.linalg.lstsq(X, y, rcond=None)[0]
        self.ax.scatter(x, y, marker='.')
        self.ax.plot(x, slope * x + intercept, color='black')
        self.ax.set_ylabel('Cumulative Runtime')
        self.ax.set_xlabel('Iteration Number')
        self.ax.yaxis.set_major_formatter(FormatStrFormatter('$%dms$'))
        self.ax.grid()


class RuntimeDensities(Plotter):
    """Creates a plots the probability density of the runtimes."""

    def plotter(self, runtimes=None, **kwargs):
        self._default_title(**kwargs)
        y = np.bincount(runtimes) / len(runtimes)
        x = np.arange(len(y))
        self.ax.xaxis.set_major_formatter(FormatStrFormatter('$%d\\mu s$'))
        mean = self.ax.axvline(x = runtimes.mean(), color = 'k', label = 'mean')
        self.ax.legend([mean], ['Runtimes'])
        self.ax.set_xlabel('Runtimes')
        self.ax.set_ylabel('Probability')
        self.ax.plot(x, y, linestyle='-')
        self.ax.grid()


class LagPlot(Plotter):
    """
    Creates a lag plot where given some runtimes it copies the array and
    shifts them by one and then plots the two data points vs each other.
    """

    def plotter(self, runtimes=None, **kwargs):
        self._default_title(**kwargs)
        x = runtimes
        y = np.roll(x, 1)
        self.ax.yaxis.set_major_formatter(FormatStrFormatter('$%d\\mu s$'))
        self.ax.xaxis.set_major_formatter(FormatStrFormatter('$%d\\mu s$'))
        self.ax.set_xlabel('Runtime at the $i$th index')
        self.ax.set_ylabel('Runtime at the $1 + i$th index')
        self.ax.scatter(x, y, marker='.')
        self.ax.grid()


def plotters_factory(dpi: Any = 100, transparent: bool = False) -> Dict[str, Plotter]:
    
    return {
        'lag-plot': lambda : LagPlot(dpi=dpi, transparent=transparent),
        'runtime-densities': lambda: RuntimeDensities(dpi=dpi, transparent=transparent),
        'cumsum-per-run': lambda: CumsumPerRun(dpi=dpi, transparent=transparent),
        'per-run': lambda: PerRun(dpi=dpi, transparent=transparent)
    }


def chunks(data: Dict, size: int):
    """Generator that makes sub-dictionaries of a maximum size."""
    it = iter(data)
    for _ in range(0, len(data), size):
        yield {k:data[k] for k in islice(it, size)}


def get_args() -> Any:
    """Gets the arguments used in the program."""
    parser = argparse.ArgumentParser(
        prog='Futhark Plots',
        description='Makes plots for futhark benchmarks.'
    )
    parser.add_argument('filename', metavar='FILE',
        help='the benchmark results as a json file generated by futhark bench.')
    parser.add_argument('--programs', metavar='PROGRAM0,PROGRAM1,...',
        help='the specific programs the plots will be generated from.')
    parser.add_argument('--plots', metavar="PLOTTYPE0,PLOTTYPE1,...",
        help=(f'the type of plots that will be generated which can be '
              f'{", ".join(plotters_factory().keys())}.'))
    parser.add_argument('--backend', default='pdf', metavar='BACKEND',
        help='the type of backend used, these can be found on the matplotlib website.')
    parser.add_argument('--filetype', default='png',  metavar='BACKEND',
        help='the file type used, these can be found on the matplotlib website.')
    parser.add_argument('--quality', default='low', metavar='QUALITY',
        choices=['low', 'medium', 'high'], help='The quality of the plot')
    parser.add_argument('--transparent', action='store_true',
        help='flag to use if the bagground should be transparent.')

    return parser.parse_args()


def format_arg_list(args: Optional[str]) -> Optional[str]:
    """
    Takes a string of form 'a, b, c, d' and makes a list ['a', 'b', 'c', 'd']
    """
    if args is None:
        return None
    
    return set(map(lambda arg: arg.strip(), args.split(',')))


def make_plot_jobs_and_directories(
    programs: Set[str],
    plot_jobs: Dict[str, Dict[str, Dict[str, Any]]],
    data: Dict[str, Dict[str, Dict[str, Any]]],
    plots: Set[str],
    filetype: str
    ) -> None:
    """Makes dictionary with plot jobs where plot_jobs are the jobs."""

    for program in programs:
        datasets = data.get(program).get('datasets')
        for dataset_path, dataset in datasets.items():
            runtimes = dataset.get('runtimes')
            directory = os.path.dirname(program)
            program_basename = pathlib.Path(program).stem
            dataset_basename = pathlib.Path(dataset_path).stem
            dataset_name = textwrap.shorten(dataset_basename, 50).replace(' ', '-')
            os.makedirs(directory, exist_ok=True)
            for plot in plots:
                filename = f'{program_basename}-{dataset_name}-{plot}.{filetype}'
                plot_file = os.path.join(directory, filename)
                plot_jobs[plot][plot_file] = {
                    'program': program_basename,
                    'dataset': dataset_basename,
                    'runtimes': np.array(runtimes)
                }

def set_quality(quality: str) -> int:
    """The quality is set based on the string given and the dpi is returned."""

    if quality == 'low':
        return 100
    elif quality == 'medium':
        return 200
    elif quality == 'high':
        plt.rcParams.update({
            'text.usetex': True,
            'font.family': 'serif',
            'font.serif': ['Computer Modern Serif']
        })
        return 400
    
    raise Exception(f'{quality} is an unknown quality.')

def main():
    plt.rcParams.update({
        'ytick.color': 'black',
        'xtick.color': 'black',
        'axes.labelcolor': 'black',
        'axes.edgecolor': 'black',
        'axes.axisbelow': True,
        'text.usetex': False,
        'axes.prop_cycle': matplotlib.cycler(color=['#5f021f'])
    })

    args = get_args()
    data = json.load(open(args.filename, 'r'))
    plots = format_arg_list(args.plots)
    programs = format_arg_list(args.programs)
    filetype = args.filetype
    quality = args.quality
    transparent = args.transparent
    matplotlib.use(args.backend)
    
    dpi = set_quality(quality)
    plotters = plotters_factory(dpi=dpi, transparent=transparent)
    plot_jobs = {k: dict() for k in plotters.keys()}

    if plots is None:
        plots = set(plot_jobs.keys())

    if programs is None:
        programs = set(data.keys())

    make_plot_jobs_and_directories(programs, plot_jobs, data, plots, filetype)

    for plot, data in plot_jobs.items():
        for data_chunk in chunks(data, max(len(data) // 4, 1)):
            plotters[plot]().plot(data_chunk)


if __name__ == '__main__':
    main()