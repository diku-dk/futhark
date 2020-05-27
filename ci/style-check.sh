#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

cd "$parent_path"

../tools/style-check.sh ../src
