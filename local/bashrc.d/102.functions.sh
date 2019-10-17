mkenv-base () 
{
    touch README.md
    cp ~/dotfiles/res/LICENSE .
}

mkenv-python ()
{
    VENV_DIR=.venv
    PACKAGE=${1:-example_package}

    # Directory setup
    mkdir $PACKAGE "test"

    touch "test/__init__.py"
    touch "$PACKAGE/__init__.py"
    cp ~/dotfiles/res/setup.py .

    python -m venv $VENV_DIR
    source $VENV_DIR/bin/activate
    pip install pip-tools
}

mkenv-go ()
{
    PACKAGE=${1:-example}

    mkdir pkg cmd
    mkdir cmd/$PACAKGE pkg/$PACKAGE
}

mkenv ()
{
  mkenv-base

  if [ ! -z "$2" ] ; then
    case $2 in
      go)          mkenv-go $1   ;;
      golang)      mkenv-go $1   ;;
      python)      mkenv-python $1   ;;
      *)           echo "'$1' env cannot be created" ;;
    esac
  else
      mkenv-python $1
  fi
}


function sorted-ls {
    python -c "import os; print('\n'.join(sorted(os.listdir())))"
}


repeat ()
{
    local times=${1:-1}
    for i in $(seq 1 $times)
    do
        eval $2
    done
}

#
# # ex - archive extractor
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1     ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}
