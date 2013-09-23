#!/bin/sh

echo Once ghci has started, use ':load Main', ':load Language.L0.Parser', or whatever else you want.

ghci $(cat .ghci | cut -d' ' -f2-)