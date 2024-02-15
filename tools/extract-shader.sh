#!/bin/bash

sed -ze 's/.*== SHADER START ==\n\(.*\)== SHADER END ==.*/\1/'
