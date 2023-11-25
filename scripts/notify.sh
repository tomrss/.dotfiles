#!/bin/sh

set -e

# needed for GNU Guix
export PATH="$PATH:/run/current-system/profile/bin:/run/current-system/profile/sbin:/run/setuid-programs"
# set display for targeting correct bus in notifications
# TODO find this one out programmatically... just hardcoded for the moment, too much hassle
export DISPLAY=":0"

usage() {
    echo "$0 -u <USER_UID> [-i <icon>] [-c] RECAP MSG"
}

# getopts
while getopts u:i:c opt; do
    case $opt in
        u)
            uid=$OPTARG
            ;;
        i)
            icon=$OPTARG
            ;;
        c)
            critical=1
            ;;
    esac
done

shift $((OPTIND - 1))

if [ -z $uid ]; then
    usage
    exit 1
fi


recap=$1
msg=$2

if [ -z "$recap" ] || [ -z "$msg" ]; then
    usage
    exit 1
fi

# get user
user=$(id -nu $uid)

# get urgency
urgency=normal
if [ $critical ]; then
    urgency=critical
fi

# Execute

if [ -z $msg ]; then
    msg=$recap
fi

if [ -z $icon ]; then
    su - $user -c "notify-send -u $urgency '$recap' '$msg'"
else
    su - $user -c "notify-send -u $urgency -i $icon '$recap' '$msg'"
fi
