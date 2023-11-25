#!/bin/sh

set -e

# needed for GNU Guix
export PATH="$PATH:/run/current-system/profile/bin:/run/current-system/profile/sbin:/run/setuid-programs"
# set display for targeting correct bus in notifications
# TODO find this one out programmatically... just hardcoded for the moment, too much hassle
export DISPLAY=":0"

# log file for logging progress of this script
log_file=/tmp/my-udev.log
touch $log_file

#
## Functions
#

# print usage
usage() {
    echo "Usage: $0 -a <action> -d <device> -u <user uid> [-m <manufacturer> -p <product>]"
}

# log into log file
log() {
    level=$1
    msg=$2

    echo "[info $(date +%F'T'%T)] $msg" >> $log_file
}

# notify the user via dbus
notify() {
    user=$1
    recap=$2
    msg=$3
    icon=$4

    # act as the user, send notification and redirect output to log file
    su - $user -c "notify-send -i $icon '$recap' '$msg' >> $log_file 2>&1"
}

#
## Parse cli options
#

# options defaults
manufacturer="Unknown"
product="Unknown"
icon=media-removable

# getopts
while getopts Aa:d:u:m:p:i:h opt; do
    case $opt in
        A)
            automount=1
            ;;
        a)
            action=$OPTARG
            ;;
        d)
            device=$OPTARG
            ;;
        u)
            uid=$OPTARG
            ;;
        m)
            manufacturer=$OPTARG
            manufacturer_known=1
            ;;
        p)
            product=$OPTARG
            ;;
        i)
            icon=$OPTARG
            ;;
        h)
            usage
            exit 0
            ;;
        *)
            usage
            log "error" "unknown option $opt"
            exit 1
            ;;
    esac
done

shift $((OPTIND - 1))

# check required options
if [ -z $action ]; then
    log "error" "required option missing: action, -a"
    usage
    exit 1
fi
if [ -z $device ]; then
    log "error" "required option missing: device, -d"
    usage
    exit 1
fi
if [ -z $uid ]; then
    log "error" "required option missing: uid, -u"
    usage
    exit 1
fi

#
## Handle device
#

log "info" "rule fired for device $device ($manufacturer) with action $action"

user=$(id -nu $uid)
# needed because we will be writing to log file as user (notify function)
chown $user $log_file
chmod +rw $log_file

if [ "$action" == "add" ]; then
    # device was plugged

    # get device info with blkid
    dev_info=$(blkid $device -o export)
    dev_fs_type=$(echo "$dev_info" | grep '^TYPE=' | cut -d '=' -f 2)
    dev_label=$(echo "$dev_info" | grep '^LABEL=' | cut -d '=' -f 2)

    if [ -z $dev_fs_type ]; then
        log "error" "unable to determine device $device fs type"
        exit 1
    fi

    # needed for mount options, cannot mount with -t ntfs otherwise fs is readonly
    if [ $dev_fs_type == 'ntfs' ]; then
        # package ntfs-3g needed for this
        dev_fs_type='ntfs-3g'
    fi

    # get a pretty name for the device
    dev_pretty_name=$(echo $dev_label | sed 's/\\//g')
    if [ -z $dev_pretty_name ]; then
        if [ $manufacturer_known ]; then
            dev_pretty_name=$manufacturer
        else
            dev_pretty_name=$product
        fi
    fi

    log "info" "Device $dev_pretty_name with fs type $dev_fs_type"

    if [ $automount ]; then
        # mount the device
        
        mount_point=/media/$user/$(echo "$dev_pretty_name" | sed -e 's/[^A-Za-z0-9._-]/_/g')
        mkdir -p "$mount_point"
        chown $user $mount_point
        mount -t "$dev_fs_type" "$device" "$mount_point" -o rw,uid=$uid >> $log_file 2>&1

        log "info" "device $device ($dev_pretty_name) mounted on $mount_point"

        # notify
        notify $user "$dev_pretty_name" "Device plugged and mounted: $product\nType: $dev_fs_type\nManufacturer: $manufacturer\nMounted on $mount_point" "$icon"
    else
        # don't mount, just notify
        notify $user "$dev_pretty_name" "Device plugged: $product\nType: $dev_fs_type\nManufacturer: $manufacturer" "$icon"
    fi

    log "info" "notification for device add fired"

elif [ "$action" == "remove" ]; then
    # device was unplugged

    # get device pretty name
    dev_pretty_name=$product
    if [ $manufacturer_known ]; then
        dev_pretty_name=$manufacturer
    fi

    # notify user
    log "info" "notifying removal of $dev_pretty_name ($product)"
    notify $user "$dev_pretty_name" "Device unplugged: $product" "$icon"
    log "info" "notification for device remove fired"
else
    log "error" "unkown action: $action"
fi
