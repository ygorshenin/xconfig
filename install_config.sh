#!/bin/bash

usage() {
    echo "Usage: $0 [-v|--verbose] [-h|--help]"
    echo "Options:"
    echo "--verbose          show all actions"
    echo "--help             show this help message"
}

verbose=
help=

process_opts() {
    for opt in "$@"; do
        case "${opt}" in
            -v|--verbose)
                verbose=true
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
XSESSION_PATH=${ROOT_DIR}/.xsession
SCRIPTS_PATH=${ROOT_DIR}/scripts

if [ ! -z "${help}" ]; then
    usage
    exit 0
fi

log info "Installing xmonad config..."
if ! cp -R ${XMONAD_PATH} ${HOME}; then
    log error "Can't install xmonad config, aborting."
    exit -1
fi

log info "Installing xmobar config..."
if ! cp ${XMOBAR_PATH} ${HOME}; then
    log error "Can't install xmobar config, aborting."
    exit -1
fi

log info "Installing xsession config..."
if ! cp ${XRESOURCES_PATH} ${XSESSION_PATH} ${HOME}; then
    log error "Can't install xsession config..."
    exit -1
fi

log info "Installing scripts..."
if ! cp -R ${SCRIPTS_PATH} ${HOME}; then
    log error "Can't install scripts, aborting."
    exit -1
fi

exit 0
