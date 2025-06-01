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

## Nyxt Source (local install)
nyxt=$XDG_DATA_HOME/common-lisp/src/nyxt

## Update PATH
export PATH="$nyxt:$PATH"
