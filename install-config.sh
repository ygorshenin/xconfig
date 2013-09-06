#!/bin/bash

usage() {
    echo "Usage: $0 [--no-xmonad] [--no-xmobar] [--no-scripts] [-v|--verbose] [-h|--help]"
    echo "Options:"
    echo "--no-xmonad    don't install xmonad configuration files"
    echo "--no-xmobar    don't install xmobar configuration files"
    echo "--no-scripts   don't install scripts"
    echo "--verbose      show all actions"
    echo "--help         show this help message"
}

verbose=
no_xmonad=
no_xmobar=
no_scripts=
help=

process_opts() {
    for opt in "$@"
    do
        case "${opt}" in
            -v|--verbose)
                verbose=true
                ;;
            --no-xmonad)
                no_xmonad=true
                ;;
            --no-xmobar)
                no_xmobar=true
                ;;
            --no-scripts)
                no_scripts=true
                ;;
            -h|--help)
                help=true
                ;;
            --)
                break
                ;;
            -*|--*)
                echo "Unknown option: ${opt}" >&2
                echo "Better safe than sorry, aborted." >&2
                exit -1
                ;;
            *)
                break
        esac
    done
}

process_opts $@

if [ ! -z "${help}" ]; then
    usage
    exit 0
fi

if [ -z "${no_xmonad}" ]; then
fi

if [ -z "${no_xmobar}" ]; then
    echo "installing xmobar config"
fi

if [ -z "${no_scripts}" ]; then
    echo "installing scripts"
fi
