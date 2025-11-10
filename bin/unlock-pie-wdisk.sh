#!/usr/bin/env sh
set -e

kill_agent() {
    eval "$(ssh-agent -k)"
}

eval "$(ssh-agent)"
trap kill_agent EXIT

ssh-add -t 60
passage wdisk/slot1 | ssh pie.local sudo cryptsetup luksOpen /dev/sda wdisk -S1
