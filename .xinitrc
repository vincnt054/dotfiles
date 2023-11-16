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

[ -f "$HOME"/.bash_profile ] && . "$HOME"/.bash_profile

setxkbmap -option caps:escape &
systemctl --user import-environment DISPLAY XAUTHORITY &

if [[ $(xrandr | awk '/DP/ {print $2}') == "connected" ]]; then
    xrandr \
	    --output "$(xrandr --listactivemonitors | awk '/HDMI/ {print $4}')" --pos 0x0 --mode 1920x1080 --primary \
		--output "$(xrandr --listactivemonitors | awk '/DP/ {print $4}')" --pos 1920x0 --mode 1920x1080 &
fi

exec i3 -c "$HOME"/.config/i3/config
