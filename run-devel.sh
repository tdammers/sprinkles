#!/bin/bash

function watch_serve() {
    echo "Watch..."
    BASEDIR="$(realpath .)"
    cd "$1"
    for ((;;))
    do
        templar "$2" &
        PID="$!"
        inotifywait \
            -e modify \
            -e attrib \
            "$(which templar)" \
            project.yml \
            templates/ \
            "$BASEDIR"/run-devel.sh || exit 255
        kill "$!"
    done
}

stack install --test
watch_serve examples/blogg 5000 &
watch_serve examples/countryInfo 5001 &
stack install --file-watch --test
