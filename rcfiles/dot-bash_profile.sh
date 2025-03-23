# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# User specific environment and startup programs

## General Env
export LC_COLLATE=C
#export EDITOR=lem
#export BROWSER=nyxt
# legacy defaults
export EDITOR=emacs
export BROWSER=zen

## XDG Env
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share

## PATHS

###Qlot Source
qlot=$HOME/.qlot/bin

#### Lem Source
lem=$XDG_DATA_HOME/common-lisp/lem

#### Common Lisp Development and Lem Hacking!
export PATH="$qlot:$lem:$PATH"
