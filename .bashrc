## Git Aliases
alias gpo="git branch | awk '/^\*/{print $NF}' | xargs git push origin"
