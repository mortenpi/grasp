#!/usr/bin/env bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd "${DIR}" || { >&2 echo "ERROR: can't enter directory ${DIR}"; exit 1; }
rm -vf rwfn.*
