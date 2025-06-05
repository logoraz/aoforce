# .bash_profile

# Get the aliases and functions
if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

# User specific environment and startup programs

## General Env
export LC_COLLATE=C
export EDITOR=emacs
export BROWSER=nyxt

## XDG Env
export XDG_CONFIG_HOME=$HOME/.config
export XDG_DATA_HOME=$HOME/.local/share

## Update PATH - Common Lisp Utilities/Applications

## Lem Source (custom install)
qlot=$HOME/.qlot/bin
lem=$XDG_DATA_HOME/common-lisp/bin/lem

## Nyxt Source (custom install)
nyxt=$XDG_DATA_HOME/common-lisp/bin/nyxt

## Update PATH
export PATH="$qlot:$lem:$nyxt:$PATH"
