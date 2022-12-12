#!/bin/bash

set -e

usage() {
    echo "Usage: $0 [command] [-d <emacs-init-directory>]"
    echo "  command:"
    echo "      - install:    install packages"
    echo "      - update:     update all packages"
    echo "      - uninstall:  uninstall all packages and delete state"
    echo "  options:"
    echo "      -d <dir>: like the --init-directory options of Emacs itself"
}

while getopts "d:rfh" opt; do
    case "$opt" in
        d)
            init_directory=$OPTARG
            ;;
        f)
            force=true
            ;;
        h)
            usage
            exit 0
            ;;
        *)
            usage
            exit 1
            ;;
    esac
done
shift $((OPTIND-1))

command="$1"

if [[ -z "$init_directory" ]]; then
    init_directory="$EMACS_HOME"
fi

if [[ -z "$init_directory" ]]; then
    echo "Missing -d option or EMACS_HOME env variable"
    usage
    exit 1
fi

# TODO check command

elisp_func="+$command"
[[ "$force" ]] && elisp_args="'force" || elisp_args="nil"
elisp="(${elisp_func} ${elisp_args})"
echo "Executing elisp $elisp"

emacs --batch --init-directory . --load update.el --eval "$elisp"
