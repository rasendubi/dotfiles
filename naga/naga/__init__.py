#!/usr/bin/env python3
import evdev
import subprocess


def value_to_string(val):
    if val == evdev.events.KeyEvent.key_up:
        return "up"
    if val == evdev.events.KeyEvent.key_down:
        return "down"
    if val == evdev.events.KeyEvent.key_hold:
        return "hold"
    return None


def main():
    device = evdev.InputDevice(
        "/dev/input/by-id/usb-Razer_Razer_Naga_Chroma-if02-event-kbd"
    )
    print(device)

    device.grab()

    for event in device.read_loop():
        if event.type == evdev.ecodes.EV_KEY:
            # subprocess.call(['awesome-client', 'razer({}, "{}")'.format(event.code - 1, value_to_string(event.value))])
            subprocess.call(
                [
                    "emacsclient",
                    "--eval",
                    '(rasen/razer {} "{}")'.format(
                        event.code - 1, value_to_string(event.value)
                    ),
                ]
            )
