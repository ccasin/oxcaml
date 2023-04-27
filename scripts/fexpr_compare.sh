#!/bin/bash

# This script is for comparing dumped flambda code in the fexpr format from two
# builds of the tree (or some subset of it).  It's assumed you're using this
# hacked branch of the compiler, which, when passed the -drawfexpr flag, will
# dump these files.
#
# They get dumped to /usr/local/home/ccasinghino/tmp/flambda-dump/... in a
# directory structure mirroring the tree.  If you aren't me, find that path in
# `middle-end/flambda2/flambda2.ml` and edit it.  Also edit the "command_to_run"
# below.
#
# You need to build twice with the two versions of the compiler you want to
# compare, (my print-flt and print-flt-vanilla branches, which you are probably
# looking at one of, are already set to dump the IR to files named
# "<file>.layouts.fl" and "<file>.vanilla.fl", which is what this script expects
# to see.  There's some kind of race condition around directory creation in my
# hack that may cause large builds to fail.  You can just restart them and it
# should work.  Doesn't seem work fixing.
#
# Then copy this script to `flambda-dump` and run it from there.  This is
# expected to print a lot of output, including errors.  The last three lines
# have a summary.  You can look at the generated "results" file to see what
# happened for each pair of files we tried to compare.  The format is the name
# of one of the files followed by the exit code of the comparison tool, which
# will be 0 on success, 42 if a difference was detected, and something else if
# it failed to parse or compare (it is incomplete and buggy).
#
# You should be able to take the last commit from branch [print-flt] in
# [github.com/ccasin/flambda-backend] and rebase it elsewhere if you want to
# test other versions of the compiler.

command_to_run="/home/ccasinghino/ocaml/layouts/flambda-backend/_build/main/middle_end/flambda2/tests/tools/fldiff.exe"
results_file="results"


# Empty or create the results file before starting
> "$results_file"

equivalent_runs=0
different_runs=0
failed_runs=0

process_files() {
  f1="$1"
  f2="${f1%.layouts.fl}.vanilla.fl"

  if [[ -e "$f2" ]]; then
    "$command_to_run" "$f1" "$f2"
    exit_code=$?
    if [[ $exit_code -eq 42 ]]; then
      # sometimes the tool reports a diff where there isn't one.  Check that
      # the files aren't literally identical, at least.
      diff "$f1" "$f2"
      diff_exit_code=$?
      if [[ $diff_exit_code -eq 0 ]]; then
        exit_code=0
      fi
    fi

    echo "$f1 $exit_code" >> "$results_file"

    if [[ $exit_code -eq 0 ]]; then
      ((equivalent_runs++))
    elif [[ $exit_code -eq 42 ]]; then
      ((different_runs++))
    else
      ((failed_runs++))
    fi
  fi
}

export -f process_files

for file in `find . -type f -name "*.layouts.fl"`; do
    process_files $file
done

echo "Equivalent files: $equivalent_runs"
echo "Different files: $different_runs"
echo "Failed runs: $failed_runs"
