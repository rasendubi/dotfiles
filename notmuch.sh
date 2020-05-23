#!/bin/sh
notmuch new
notmuch tag --input "$(dirname "$0")/notmuch-tags"
notmuch tag +muted -unread $(notmuch search --output=threads tag:muted)
