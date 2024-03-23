# Firejail profile for discord
# This file is overwritten after every install/update
# Persistent local customizations
include discord.local
# Persistent global definitions
include globals.local

noblacklist ${HOME}/.config/discord

mkdir ${HOME}/.config/discord
whitelist ${HOME}/.config/discord
whitelist ${HOME}/Desktop
whitelist ${HOME}/Documents
whitelist ${HOME}/Downloads
whitelist ${HOME}/Music
whitelist ${HOME}/Public
whitelist ${HOME}/Pictures
whitelist ${HOME}/Videos
whitelist ${HOME}/library

private-bin discord,Discord
private-opt discord,Discord

# Redirect
include discord-common.profile
