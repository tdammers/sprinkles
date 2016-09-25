#!/bin/bash

# Convenience script for interactive-ish development. Starts up some background
# processes to constantly update and restart things.
#
# Specifically:
# - Recompile on source change
# - Restart servers on relevant changes
# - Re-run tests on source change
# - Rebuild haddock on source change
# - Rebuild tags file on source change

function watch_serve() {
    BASEDIR="$(realpath .)"
    cd "$1"
    for ((;;))
    do
        sleep 1
        pgrep -l templar
        templar "$2" &
        PID="$!"
        echo "$PID: $1 :$2"
        pgrep -l templar
        inotifywait \
            -e attrib \
            "$(which templar)" \
            project.yml \
            templates/** \
            $(find -name static) \
            "$BASEDIR"/run-devel.sh
        sleep 1
        kill "$PID"
    done
}

function watch_hasktags() {
    for ((;;))
    do
        inotifywait \
            -e modify \
            src/**/*.hs app/*.hs test
        hasktags . -c
    done
}

stack install # --test --haddock
watch_serve examples/blogg 5100 &
watch_serve examples/countryInfo 5101 &
watch_serve examples/playground 5102 &
watch_hasktags &
stack install --file-watch # --test --haddock
