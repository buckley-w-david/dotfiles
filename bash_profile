# do not edit this file. put files in the dir below.
export TRACE="/home/david/.bash_profile:$TRACE"

source ~/.profile
source ~/.bashrc

old_nullglob=$(shopt -p | grep 'nullglob$')
shopt -s nullglob
files=($HOME/.config/bash_profile.d/*.{sh,bash})
eval "$old_nullglob"
unset -v old_nullglob

for FN in ${files[*]} ; do
    source "$FN"
done
