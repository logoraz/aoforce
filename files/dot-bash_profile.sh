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
export XDG_CACHE_HOME=$HOME/.cache
export XDG_DATA_HOME=$HOME/.local/share
export XDG_BIN_HOME=$HOME/.local/bin

## Update PATH
# export PATH="lem:$PATH"
