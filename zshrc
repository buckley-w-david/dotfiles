# Created by newuser for 5.8
precmd() {
  psvar[1]=$(fish_path "$(pwd)")
}

PROMPT='%F{green}%n%f@%m %F{green}%1v%f%(?.. %F{red}[%?]%f)> '

alias ls='ls --color'
alias ll='ls --color -lisah'
