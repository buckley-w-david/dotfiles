# do not edit this file. put files in the dir below.
export TRACE="/home/david/.bash_profile:$TRACE"

source ~/.bashrc
for FN in $HOME/.local/bash_profile.d/*.sh ; do
    source "$FN"
done