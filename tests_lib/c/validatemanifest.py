#!/usr/bin/env python3

from jsonschema import validate
import json
import sys

schema = json.load(open(sys.argv[1]))
manifest = json.load(open(sys.argv[2]))

validate(instance=manifest, schema=schema)
