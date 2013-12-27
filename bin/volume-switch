#!/bin/bash

command="$1"
if [ -z "${command}" ]; then
    exit -1
fi

case "${command}" in
    lower)
	action="2-"
	;;
    raise)
	action="2+"
	;;
    toggle)
	action="toggle"
	;;
    *)
	exit -1
esac

DIR=$(dirname "$(realpath $0)")
SCRIPT=${DIR}/amixer_status.awk
VOLUME=/tmp/.volume

amixer set Master "${action}" 2>&1 >/dev/null

if [ ! -e "${VOLUME}" ] || [ ! -p "${VOLUME}" ]; then
    rm -rf "${VOLUME}"
    mkfifo "${VOLUME}"
fi

volume=$(amixer get Master | awk -f "${SCRIPT}")
echo ${volume}
echo "${volume}" >"${VOLUME}" &
