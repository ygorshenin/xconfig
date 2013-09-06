#!/bin/bash

usage() {
    echo "Usage: $0 [--no-xmonad] [--no-xmobar] [--no-scripts] [-v|--verbose] [-h|--help]"
    echo "Options:"
    echo "--no-xmonad        don't install xmonad configuration files"
    echo "--no-xmobar        don't install xmobar configuration files"
    echo "--no-xsession      don't install xsession configuration files"
    echo "--no-scripts       don't install scripts"
    echo "--verbose          show all actions"
    echo "--help             show this help message"
}

verbose=
no_xmonad=
no_xmobar=
no_xsession=
no_scripts=
help=

process_opts() {
    for opt in "$@"; do
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
            --no-xsession)
                no_xsession=true
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

log() {
    local level="$1"
    local message="$2"
    case "${level}" in
        info)
            if [ ! -z "${verbose}" ]; then
                echo "INFO: ${message}" >&1
            fi
            ;;
        error)
            echo "ERROR: ${message}" >&2
            ;;
        *)
            ;;
    esac
}

process_opts $@

ROOT_DIR=$(dirname "$(realpath $0)")
XMONAD_PATH=${ROOT_DIR}/.xmonad
XMOBAR_PATH=${ROOT_DIR}/.xmobarrc
XRESOURCES_PATH=${ROOT_DIR}/.Xresources
SCRIPTS_PATH=${ROOT_DIR}/scripts

if [ ! -z "${help}" ]; then
    usage
    exit 0
fi

if [ -z "${no_xmonad}" ]; then
    log info "Installing xmonad config..."
    if ! cp -R ${XMONAD_PATH} ${HOME}; then
        log error "Can't install xmonad config, aborting."
        exit -1
    fi
fi

if [ -z "${no_xmobar}" ]; then
    log info "Installing xmobar config..."
    if ! cp ${XMOBAR_PATH} ${HOME}; then
        log error "Can't install xmobar config, aborting."
        exit -1
    fi
fi

if [ -z "${no_xsession}" ]; then
    log info "Installing xsession config..."
    if ! cp ${XRESOURCES_PATH} ${HOME}; then
        log error "Can't install xsession config..."
        exit -1
    fi
fi

if [ -z "${no_scripts}" ]; then
    log info "Installing scripts..."
    if ! cp -R ${SCRIPTS_PATH} ${HOME}; then
        log error "Can't install scripts, aborting."
        exit -1
    fi
fi

exit 0
