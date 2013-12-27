#!/bin/bash

usage() {
    echo "Usage: $0 [-c dir] [-u dir] [-h]"
    echo "Options:"
    echo "-c                whether sources should be downloaded"
    echo "-u                whether existing repository should be updated"
    echo "-h                this help message"
}

check_directory() {
    local option=$1 directory=$2
    if [ -z "${directory}" ]; then
        echo "Invalid -${option} option"
        usage
        exit 1
    fi
    if [ ! -x "${directory}" ] || [ ! -d "${directory}" ]; then
        echo "Invalid directory ${directory}"
        exit 1
    fi
}

process_opts() {
    local OPTNAME OPTARG
    while getopts ":c:u:h" OPTNAME "$@"; do
        case "${OPTNAME}" in
            c)
                check_directory "${OPTNAME}" "${OPTARG}"
                checkout_directory="${OPTARG}"
                ;;
            u)
                check_directory "${OPTNAME}" "${OPTARG}"
                repository_directory="${OPTARG}"
                ;;
            h)
                usage
                exit 0
                ;;
            \:)
                echo "'-${OPTARG}' needs an argument."
                usage
                exit 1
                ;;
            *)
                echo "Invalid command-line option: ${OPTARG}"
                usage
                exit 1
                ;;
        esac
    done
}

process_opts "$@"

if [ ! -z "${checkout_directory}" ]; then
    set -e
    set -x
    (
        cd "${checkout_directory}"
        git clone http://llvm.org/git/llvm.git
        (cd llvm/tools && git clone http://llvm.org/git/clang.git)
        (cd llvm/projects && git clone http://llvm.org/git/compiler-rt.git)
        (cd llvm/projects && git clone http://llvm.org/git/test-suite.git)
        git config branch.master.rebase true
    )
    set +e
    set +x
fi
if [ ! -z "${repository_directory}" ]; then
    set -e
    set -x
    (
        cd "${repository_directory}"
        git pull --rebase
        (cd "tools/clang" && git pull --rebase)
        (cd "projects/compiler-rt" && git pull --rebase)
    )
    set +e
    set +x
fi

exit 0
