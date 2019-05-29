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
        pgrep -l sprinkles
        sprinkles -serve "$2" &
        PID="$!"
        echo "$PID: $1 :$2"
        pgrep -l sprinkles
        inotifywait \
            -e attrib \
            "$(which sprinkles)" \
            project.yml \
            templates/** \
            $(find -name static)
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

function build() {
    cabal new-build && cabal-install-newbuilt sprinkles
}

function watch_build() {
    for ((;;))
    do
        echo 'Watching'
        inotifywait \
            -e modify \
            -r \
            src app \
            sprinkles.cabal \
            --exclude '(/|^)\.'
        build
    done
}

build
watch_serve examples/blogg 5100 &
watch_serve examples/countryInfo 5101 &
watch_serve examples/playground 5102 &
watch_hasktags &
watch_build
