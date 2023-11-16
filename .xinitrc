#!/bin/sh
userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

if [ -f $sysresources ]; then
    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

[ -f $HOME/.bash_profile ] && . $HOME/.bash_profile

setxkbmap -option caps:escape &
systemctl --user import-environment DISPLAY XAUTHORITY &

if [[ $(xrandr | awk '/DP-1/ {print $2}') == "connected" ]]; then
    xrandr \
		--output HDMI-0 --pos 0x0 --mode 1920x1080 --primary \
		--output DP-1 --pos 1920x0 --mode 1920x1080 &
    exec i3 -c $HOME/.config/i3/config_dual
else
    exec i3 -c $HOME/.config/i3/config
fi
