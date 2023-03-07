#!/usr/bin/env python3

import sys
assert sys.version_info >= (3, 9), "Use Python 3.9 or newer."

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
from matplotlib.ticker import FormatStrFormatter
from multiprocessing import Pool
from abc import ABC, abstractmethod
from typing import Any, Dict, Optional, Set, List
from itertools import islice
from PIL import ImageFile
from collections import OrderedDict
ImageFile.LOAD_TRUNCATED_IMAGES = True


def to_file_path(dest: str, plot_type_name: str, extension: str) -> str:
    '''
    Will create the path to a given file based on the plot type and extension.
    '''

    return f'{dest}_{plot_type_name}.{extension}'


class PlotType(ABC):

    def _default_title(self, ax, program: str = None, dataset: str = None):
        '''Sets the title for the current axes.'''
        if dataset is None or program is None:
            return

        ax.set_title(textwrap.shorten(fr'{program}: {dataset}',  50))

    @abstractmethod
    def plot(self, ax, **kwargs):
        '''Method used to create a plot.'''
        raise NotImplementedError()

    @classmethod
    @abstractmethod
    def name(cls):
        '''The name of the plot.'''
        raise NotImplementedError()


class PerRun(PlotType):
    '''Create a plot with runtime vs iteration number as plot.'''

    def plot(self, ax, runtimes=None, **kwargs):
        self._default_title(ax, **kwargs)
        x = np.arange(len(runtimes))
        y = runtimes
        runtimes = ax.scatter(x, y, marker='.')
        ax.legend([runtimes], ['Runtimes'])
        ax.yaxis.set_major_formatter(FormatStrFormatter('$%d\\mu s$'))
        ax.set_ylabel('Runtime')
        ax.set_xlabel('$i$th Runtime')
        ax.grid()

    @classmethod
    def name(cls) -> str:
        return 'per_run'


class CumsumPerRun(PlotType):
    '''Create a plot with cumulative runtime vs iteration number as plot.'''

    def plot(self, ax, runtimes=None, **kwargs):
        self._default_title(ax, **kwargs)
        x = np.arange(len(runtimes))
        y = np.cumsum(runtimes) / 1000
        X = np.vstack([x, np.ones(len(x))]).T
        slope, intercept = np.linalg.lstsq(X, y, rcond=None)[0]
        ax.scatter(x, y, marker='.')
        ax.plot(x, slope * x + intercept, color='black')
        ax.set_ylabel('Cumulative Runtime')
        ax.set_xlabel('$i$th Runtime')
        ax.yaxis.set_major_formatter(FormatStrFormatter('$%dms$'))
        ax.grid()

    @classmethod
    def name(cls) -> str:
        return 'cumsum_per_run'


class RuntimeDensities(PlotType):
    '''Creates a plots the probability density of the runtimes.'''

    def plot(self, ax, runtimes=None, **kwargs):
        self._default_title(ax, **kwargs)
        y = np.bincount(runtimes) / len(runtimes)
        x = np.arange(len(y))
        ax.xaxis.set_major_formatter(FormatStrFormatter('$%d\\mu s$'))
        mean = ax.axvline(x = runtimes.mean(), color = 'k', label = 'mean')
        ymin = y.min()
        ymax = y.max()
        padding = abs(ymax - ymin) * 0.05
        ax.set_ylim(ymin - padding, ymax + padding)
        ax.legend([mean], ['Mean Runtime'])
        ax.set_xlabel('Runtime')
        ax.set_ylabel('Probability')
        ax.plot(x, y, linestyle='-')
        ax.grid()

    @classmethod
    def name(cls) -> str:
        return 'runtime_densities'


class LagPlot(PlotType):
    '''
    Creates a lag plot where given some runtimes it copies the array and
    shifts them by one and then plots the two data points vs each other.
    '''

    def plot(self, ax, runtimes=None, **kwargs):
        self._default_title(ax, **kwargs)
        x = runtimes
        y = np.roll(x, 1)
        ax.yaxis.set_major_formatter(FormatStrFormatter('$%d\\mu s$'))
        ax.xaxis.set_major_formatter(FormatStrFormatter('$%d\\mu s$'))
        ax.set_xlabel('The $i$th Runtime')
        ax.set_ylabel('The $i$th + 1 Runtime')
        ax.scatter(x, y, marker='.')
        ax.grid()

    @classmethod
    def name(cls) -> str:
        return 'lag_plot'


ALL_PLOT_TYPES = OrderedDict({
    plot_type.name():  plot_type for plot_type in PlotType.__subclasses__()
})

class Plotter:
    '''Class that will plot and save many figures on a process.'''

    def __init__(
            self,
            plot_types: List[PlotType],
            dpi: Any = '200',
            transparent: bool = False,
            backend='AGG') -> None:
        self.dpi = dpi
        self.plot_types = plot_types
        self.fig, self.ax = plt.subplots()
        self.transparent = transparent
        backends = {
            'AGG': 'png',
            'PDF': 'pdf',
            'PS': 'ps',
            'PS': 'eps',
            'SVG': 'svg',
            'PGF': 'pgf'
        }
        self.filetype = backends[backend]
        self.backend = backend

    def plot(self, data: Dict[str, Any]):
        '''
        Will use the plotter function on all the given data. The data is a
        dictionary where the key is the destination and the value is the values
        that will be passed to the plotter function.
        '''

        for plotter in self.plot_types:
            for dest, datapoint in data.items():
                plotter.plot(self.ax, **datapoint)

                # For some reason a syntax error may occour but the savefig function
                # will for some reason work afterwards.
                for _ in range(10):
                    try:
                        path = to_file_path(dest, plotter.name(), self.filetype)
                        self.fig.savefig(
                            path,
                            bbox_inches='tight',
                            dpi=self.dpi,
                            backend=self.backend,
                            transparent=self.transparent
                        )
                        print(f'Done creating: {path}')
                        break
                    except SyntaxError:
                        time.sleep(1)
                else:
                    print((f'Figure "{dest}" did not get saved this is most likely an internal '
                           'error. try specifying the specific program with --program.'))

                self.ax.cla()
        plt.close(self.fig)


def chunks(data: Dict, size: int):
    '''Generator that makes sub-dictionaries of a maximum size.'''
    it = iter(data)
    for _ in range(0, len(data), size):
        yield {k:data[k] for k in islice(it, size)}


def get_args() -> Any:
    '''Gets the arguments used in the program.'''
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
              f'{", ".join(ALL_PLOT_TYPES.keys())}. Default is all plots.'))
    parser.add_argument('--backend', default='pdf', metavar='BACKEND',
        help='the type of backend used, these can be found on the matplotlib website.')
    parser.add_argument('--filetype', default='png',  metavar='BACKEND',
        help='the file type used, these can be found on the matplotlib website.')
    parser.add_argument('--transparent', action='store_true',
        help='flag to use if the bagground should be transparent.')

    return parser.parse_args()


def format_arg_list(args: Optional[str]) -> Optional[str]:
    '''
    Takes a string of form 'a, b, c, d' and makes a list ['a', 'b', 'c', 'd']
    '''
    if args is None:
        return None

    return set(map(lambda arg: arg.strip(), args.split(',')))


def iter_datasets(
    data: Dict[str, Dict[str, Dict[str, Any]]],
    programs: Set[str]):
    '''Makes iteration easier for make_plot_jobs_and_directories.'''

    for program_path in programs:
        datasets = data.get(program_path).get('datasets')
        program = pathlib.Path(program_path).name.replace('.fut', '').replace('.in', '')
        program_directory = os.path.dirname(program_path)
        for dataset_path, dataset_dict in datasets.items():
            dataset_directory = os.path.dirname(dataset_path)
            directory = os.path.join(program_directory, dataset_directory)
            directory = '.' if directory == '' else directory
            dataset = pathlib.Path(dataset_path).name
            runtimes = np.array(dataset_dict.get('runtimes'))
            memory = dataset_dict.get('bytes')
            yield  program_path, directory, program, dataset, runtimes, memory


def make_plot_jobs_and_directories(
    programs: Set[str],
    data: Dict[str, Dict[str, Dict[str, Any]]],
    plot_types: List[str],
    file_type: str,
    exnteded_path: str = 'graphs') -> Dict[str, Dict[str, Dict[str, Any]]]:
    '''Makes dictionary with plot jobs where plot_jobs are the jobs.'''

    html_data = dict()
    plot_jobs = dict()

    def remove_characters(characters, text):

        rep = {re.escape(k): '' for k in characters}
        pattern = re.compile("|".join(rep.keys()))
        return pattern.sub(lambda m: rep[re.escape(m.group(0))], text)

    for program_path, directory, program, dataset, runtimes, memory in iter_datasets(data, programs):
        os.makedirs(directory, exist_ok=True)

        if html_data.get(program_path) is None:
            html_data[program_path] = dict()

        dataset_filename = textwrap.shorten(dataset, 100).replace(' ', '_')
        raw_filename =  f'{program}_{dataset_filename}'
        filename = remove_characters([' ', '#', '"'], raw_filename)
        plot_file = os.path.join(directory, filename)
        mean = runtimes.mean()
        mpe = 100 / runtimes.shape[0] * ((runtimes - mean) / runtimes).sum()
        bound = 0.95 * runtimes.std(ddof=1) / np.sqrt(runtimes.shape[0])
        html_data[program_path][dataset] = {
            'plots': [to_file_path(plot_file, plot_type, file_type) for plot_type in plot_types],
            'bytes': memory,
            'MPE': mpe,
            'ci': (mean - bound, mean + bound)
        }
        plot_jobs[plot_file] = {
            'program': program,
            'dataset': dataset,
            'runtimes': runtimes
        }

    return html_data, plot_jobs


def make_html(html_data: Dict[str, Dict[str, Dict[str, Any]]]):
    '''Makes a simpel html document with links to each section with plots.'''

    lowercase = lambda x: x[0].casefold()

    def li(id, text):
        return rf'<li><a href=#{id}>{text}</a></li>'

    def subsection(id, data, dataset):
        plot_files = data['plots']
        mpe = data['MPE']
        memory = data['bytes']
        ci = data['ci']
        plots = ''.join(map(lambda path: f'<img src=\'{path}\'/>', plot_files))
        measures = f"""<div style="width: 100%; display: table;" align="center">
    <div style="display: table-row">
        Mean Percentage Error: {mpe}%
    </div>
    <div style="display: table-row">
        Memory Usage: {','.join([f'{b}bytes@{device}' for device, b in memory.items()])}
    </div>
    <div style="display: table-row">
        95% Confidence Interval: [{ci[0]}; {ci[1]}]
    </div>
</div>"""
        return rf'<section id={id}><h3>{dataset}</h3>{plots}{measures}</section>'

    def dataset_html(program, dataset, data):
        id = ''.join(e for e in (program + dataset) if e.isalnum()) + str(random.randint(0, 10**10))
        return li(id, dataset), subsection(id, data, dataset)

    def program_html(program, datasets):
        id = ''.join(e for e in program if e.isalnum()) + str(random.randint(0, 10**10))
        sub = map(lambda a: dataset_html(program, *a), sorted(datasets.items(), key = lowercase))
        sub_lis, sub_sections = map(lambda a: ''.join(a), zip(*sub))
        sub_lis_html = rf'<li><a href=#{id}>{program}</a><ul>{sub_lis}</ul></li>'
        sub_subsections_html = rf'<section id={id}><h2>{program}</h2>{sub_sections}</section>'
        return sub_lis_html, sub_subsections_html

    width = 0
    for program in html_data.values():
        for dataset in program.values():
            width = int(100 / len(dataset['plots']))
            break

    programs = map(lambda a: program_html(*a), sorted(html_data.items(), key = lowercase))
    lis, sections = map(lambda a: ''.join(a), zip(*programs))

    return f'''<head>
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
</head>'''


def task(data):
    '''Begins plotting, it is used'''
    global plots
    plot_types = [plot_type() for key, plot_type in ALL_PLOT_TYPES.items() if key in plot_types_used]
    plotter = Plotter(plot_types, dpi=200, transparent=transparent)
    plotter.plot(data)


def main():
    global plot_types_used
    global transparent
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
    programs = format_arg_list(args.programs)
    plot_types_used = format_arg_list(args.plots)
    filetype = args.filetype
    transparent = args.transparent
    matplotlib.use(args.backend)

    if plot_types_used is None:
        plot_types_used = set(ALL_PLOT_TYPES.keys())

    if programs is None:
        programs = set(data.keys())

    html_data, plot_jobs = make_plot_jobs_and_directories(programs, data, plot_types_used, filetype)

    with Pool(16) as p:
        p.map(task, chunks(plot_jobs, max(len(plot_jobs) // 32, 1)))

    with open('index.html', 'w') as fp:
        fp.write(make_html(html_data))

    print('Done!  Open index.html in a browser to view plots.')

if __name__ == '__main__':
    main()
