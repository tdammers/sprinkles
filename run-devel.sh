#!/bin/bash

function watch_serve() {
    echo "Watch..."
    cd example-project
    for ((;;))
    do
        templar-exe &
        inotifywait \
            -e modify \
            -e attrib \
            "$(which templar-exe)" \
            project.yml \
            templates \
            templates/include \
            ../run-devel.sh || exit 255
        killall templar-exe
    done
}

watch_serve &
stack install --file-watch --test
