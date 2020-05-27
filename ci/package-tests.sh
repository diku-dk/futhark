#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

parent_path=$( cd "$(dirname "${BASH_SOURCE[0]}")" ; pwd -P )

cd "$parent_path"

echo "# Package tests"
cd ../pkgtests
sh test.sh
