#!/usr/bin/env python
#
# Perform high-pass filtering on an image.

import argparse
import sys
import imageio
import fft
import numpy as np

parser = argparse.ArgumentParser(description='Perform high-pass filtering.')

parser.add_argument('--cutoff', metavar='INT', type=int, default=10)
parser.add_argument('input_image', type=str, metavar='INPUT_FILE')
parser.add_argument('output_image', type=str, metavar='OUTPUT_FILE')
args = parser.parse_args()

fft = fft.fft()

input = np.array(imageio.imread(args.input_image))[:,:,0:3]

output = fft.highpass_filter(args.cutoff, input).get()
imageio.imwrite(args.output_image, output)
