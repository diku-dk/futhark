#!/bin/sh

futhark eval -f test.fut '1 + 1' 'iota 4' 'plus2_iota 15'
