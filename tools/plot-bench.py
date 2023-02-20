#!/usr/bin/env python3

import sys
assert sys.version_info >= (3, 9), "Use Python 3.9 or newer."

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
from multiprocessing import Pool
from abc import ABC, abstractmethod
from typing import Union, Any, Dict, Optional, Set
from itertools import islice
from PIL import ImageFile
ImageFile.LOAD_TRUNCATED_IMAGES = True


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

        self.ax.set_title(textwrap.shorten(fr'{program}: {dataset}',  50))

    def plot(self, data: Dict[Union[io.BufferedWriter, str], Any]):
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
                    print(f'Done creating: {dest}')
                    break
                except SyntaxError:
                    time.sleep(1)
            else:
                print((f'Figure "{dest}" did not get saved this is most likely an internal error.'
                    'try specifying the specific program with --program.'))
            
            self.ax.cla()
        plt.close(self.fig)


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
    
    def __str__(self) -> str:
        return 'Per Run Plot'


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
        self.ax.legend([mean], ['Mean Runtime'])
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


def plotters_factory(dpi: Any = 200, transparent: bool = False) -> Dict[str, Plotter]:
    """A dictionary where each key value pair creates a Plotter."""

    return {
        'lag_plot': lambda : LagPlot(dpi=dpi, transparent=transparent),
        'runtime_densities': lambda: RuntimeDensities(dpi=dpi, transparent=transparent),
        'cumsum_per_run': lambda: CumsumPerRun(dpi=dpi, transparent=transparent),
        'per_run': lambda: PerRun(dpi=dpi, transparent=transparent)
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
        help='the specific programs the plots will be generated from. Default is all programs.')
    parser.add_argument('--plots', metavar="PLOTTYPE0,PLOTTYPE1,...",
        help=(f'the type of plots that will be generated which can be '
              f'{", ".join(plotters_factory().keys())}. Default is all plots.'))
    parser.add_argument('--backend', default='pdf', metavar='BACKEND',
        help='the type of backend used, these can be found on the matplotlib website.')
    parser.add_argument('--filetype', default='png',  metavar='BACKEND',
        help='the file type used, these can be found on the matplotlib website.')
    parser.add_argument('--transparent', action='store_true',
        help='flag to use if the bagground should be transparent.')
    parser.add_argument('--output', help='the output path')

    return parser.parse_args()


def format_arg_list(args: Optional[str]) -> Optional[str]:
    """
    Takes a string of form 'a, b, c, d' and makes a list ['a', 'b', 'c', 'd']
    """
    if args is None:
        return None
    
    return set(map(lambda arg: arg.strip(), args.split(',')))


def iter_datasets(
    data: Dict[str, Dict[str, Dict[str, Any]]],
    programs: Set[str]):
    """Makes iteration easier for make_plot_jobs_and_directories."""
    
    for program_path in programs:
        datasets = data.get(program_path).get('datasets')
        program = pathlib.Path(program_path).name.replace('.fut', '')
        program_directory = os.path.dirname(program_path)
        for dataset_path, dataset_dict in datasets.items():
            dataset_directory = os.path.dirname(dataset_path)
            directory = os.path.join(program_directory, dataset_directory)
            directory = '.' if directory == '' else directory
            dataset = pathlib.Path(dataset_path).name
            runtimes = np.array(dataset_dict.get('runtimes'))
            yield directory, program, dataset, runtimes


def make_plot_jobs_and_directories(
    programs: Set[str],
    plot_jobs: Dict[str, Dict[str, Dict[str, Any]]],
    data: Dict[str, Dict[str, Dict[str, Any]]],
    plots: Set[str],
    filetype: str,
    exnteded_path: str = 'assets') -> Dict[str, Dict[str, Dict[str, Any]]]:
    """Makes dictionary with plot jobs where plot_jobs are the jobs."""
    
    html_data = dict()

    def remove_characters(characters, text):

        rep = {re.escape(k): '' for k in characters}
        pattern = re.compile("|".join(rep.keys()))
        return pattern.sub(lambda m: rep[re.escape(m.group(0))], text)

    for directory, program, dataset, runtimes in iter_datasets(data, programs):
        
        directory = os.path.join(exnteded_path, directory)
        os.makedirs(directory, exist_ok=True)

        if html_data.get(program) is None:
            html_data[program] = dict() 
        
        html_data[program][dataset] = []
        
        for plot in sorted(plots):
            dataset_filename = textwrap.shorten(dataset, 100).replace(' ', '_')
            raw_filename =  f'{program}_{dataset_filename}_{plot}.{filetype}'
            filename = remove_characters([' ', '#', '"'], raw_filename)
            plot_file = os.path.join(directory, filename)
            plot_jobs[plot][plot_file] = {
                'program': program,
                'dataset': dataset,
                'runtimes': runtimes
            }

            html_data[program][dataset].append(plot_file)
    
    return html_data


def make_html(html_data: Dict[str, Dict[str, Dict[str, Any]]]):
    """Makes a simpel html document with links to each section with plots."""
    
    def li(id, text):
        return rf'<li><a href=#{id}>{text}</a></li>'

    def section(id, plot_files):
        plots = ''.join(map(lambda path: f'<img src=\'{path}\'/>', plot_files))
        return rf'<section id={id}>{plots}</section>'
    
    width = 0
    for datasets in html_data.values():
        for plot_files in datasets.values():
            width = int(100 / len(plot_files))
            break

    id_num = 0
    lis = []
    sections = []
    lowercase = lambda x: x[0].casefold()
    for program, datasets in sorted(html_data.items(), key = lowercase):
        for dataset, plot_files in sorted(datasets.items(), key = lowercase):
            id = ''.join(e for e in (program + dataset) if e.isalnum()) + str(id_num)
            lis.append(li(id, f'{program}, {dataset}'))
            sections.append(section(id, plot_files))
            id_num += 1

    lis_str = ''.join(lis)
    sections_str = ''.join(sections)

    return f"""<head>
    <style type="text/css">
        img {{
            width: {width}%
        }}
    </style>
    <nav>
        <ul>
            {lis_str}
        </ul>
    </nav>
    {sections_str}
</head>"""


def task(plot, data):
    """Begins plotting, it is used"""
    global plotters
    plotters[plot]().plot(data)

def main():
    global plotters
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
    transparent = args.transparent
    matplotlib.use(args.backend)
    
    plotters = plotters_factory(transparent=transparent)
    plot_jobs = dict({k: dict() for k in plotters.keys()})

    if plots is None:
        plots = set(plot_jobs.keys())

    if programs is None:
        programs = set(data.keys())

    html_data = make_plot_jobs_and_directories(programs, plot_jobs, data, plots, filetype)
    
    def plot_jobs_chunks(plot_jobs, size):
        for plot, data in plot_jobs.items():
            for data_chunk in chunks(data, size):
                yield plot, data_chunk

    processes = 16
    total_benchmarks = len(plot_jobs[next(iter(plot_jobs))])
    plottypes = len(plots)
    chunksize = (total_benchmarks * plottypes) // (2 * processes)

    with open('index.html', 'w') as fp:
        fp.write(make_html(html_data))
    
    with Pool(processes) as p:
        p.starmap(task, plot_jobs_chunks(plot_jobs, max(chunksize, 1)))

if __name__ == '__main__':
    main()
