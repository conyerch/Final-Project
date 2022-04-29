#!/usr/bin/env bash
rm -f submission.zip
zip -q -x "src/_build/*" -x "src/_build/*" -X -r submission.zip src
