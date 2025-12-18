#!/usr/bin/env sh
set -ex

kill_agent() {
    eval "$(ssh-agent -k)"
}
trap kill_agent EXIT

eval "$(ssh-agent)"

ssh-add -t 60
passage wdisk/slot1 | ssh pie.local sudo cryptsetup luksOpen /dev/sda wdisk -S1
