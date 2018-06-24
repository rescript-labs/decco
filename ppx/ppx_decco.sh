#!/usr/bin/env bash
pwd
${BASH_SOURCE%/*}/../_build/default/.ppx/ppx_decco/ppx.exe --as-ppx $@