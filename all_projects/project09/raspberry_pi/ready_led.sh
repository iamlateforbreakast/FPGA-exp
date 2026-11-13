#!/bin/bash
# Show on an LED wired to a GPIO whether the Pi is on Wi-Fi with sshd
# running, i.e. whether you can ssh into it yet.
#
#   blinking - booted, waiting for Wi-Fi / an IP address / sshd
#   solid on - ready: wlan0 has an IPv4 address and sshd is active
#
# The LED is registered as /sys/class/leds/ready by this line in
# /boot/firmware/config.txt (GPIO17 = header pin 11):
#
#   dtoverlay=gpio-led,gpio=17,label=ready,trigger=timer
#
# trigger=timer makes the kernel blink it from early boot, before this
# script starts. Installed as a systemd service, see ready-led.service.

IFACE=${IFACE:-wlan0}
led=/sys/class/leds/${LED:-ready}

if [ ! -d "$led" ]; then
    echo "$led not found: add the gpio-led overlay to config.txt and reboot" >&2
    exit 1
fi

set_led() {
    case $1 in
        ready)   echo default-on > "$led/trigger" ;;
        waiting) echo timer > "$led/trigger"
                 echo 250 > "$led/delay_on"
                 echo 250 > "$led/delay_off" ;;
        off)     echo none > "$led/trigger"
                 echo 0 > "$led/brightness" ;;
    esac
}

is_ready() {
    ip -4 -o addr show dev "$IFACE" 2>/dev/null | grep -q ' inet ' &&
        systemctl is-active --quiet ssh
}

trap 'set_led off; exit 0' TERM INT

state=
while true; do
    if is_ready; then new=ready; else new=waiting; fi
    if [ "$new" != "$state" ]; then
        state=$new
        set_led "$state"
        logger -t ready-led "$IFACE: $state"
    fi
    sleep 2
done
