## Git Aliases
alias gpo="git branch | grep \* | cut -c3- | xargs git push origin"

alias gs='git status '
alias ga='git add '
alias gb='git branch '
alias gc='git commit'
alias gd='git diff'
# alias go='git checkout '
alias gp='git push '

alias got='git '
alias get='git '

## Generic Aliases
alias xmonad-config="vim ~/.xmonad/xmonad.hs; xmonad --recompile; xmonad --restart"
alias ssh="kitty +kitten ssh"
