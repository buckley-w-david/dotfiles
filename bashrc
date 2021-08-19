# do not edit this file. put files in the dir below.

export TRACE="/home/david/.bashrc:$TRACE"

old_nullglob=$(shopt -p | grep 'nullglob$')
shopt -s nullglob
files=($HOME/.config/bashrc.d/*.{sh,bash})
eval "$old_nullglob"
unset -v old_nullglob

for FN in ${files[*]} ; do
    source "$FN"
done
