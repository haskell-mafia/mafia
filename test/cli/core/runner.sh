#!/usr/bin/env bash
PROJECT="${1:-./dist/build/project/project}"

type "$PROJECT" > /dev/null 2>&1 || {
    echo "No project executable specified on command line or on path."
    exit 1
}


cleanup () {
    echo "Cleaning up"
}

trap cleanup SIGHUP SIGINT SIGQUIT SIGTERM

banner () {
    echo
    echo == "$*" ==
    echo
}

exit_cleanup() {
    cleanup
    exit ${1:-1}
}
