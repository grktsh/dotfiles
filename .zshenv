setopt no_global_rcs

export LANG=ja_JP.UTF-8

typeset -U path manpath
export MANPATH

# Homebrew
path=(/usr/local/bin(N-/) /usr/local/sbin(N-/) $path)
manpath=(/usr/local/share/man(N-/) $manpath)
manpath=(/usr/local/opt/erlang/lib/erlang/man(N-/) $manpath)

# direnv
if which direnv > /dev/null; then eval "$(direnv hook zsh)"; fi

# Ruby
if which rbenv > /dev/null; then eval "$(rbenv init -)"; fi

# Python
export PYTHON_BUILD_SKIP_MIRROR=1
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
if which pyenv > /dev/null; then eval "$(pyenv init -)"; fi
if which pyenv-virtualenv > /dev/null; then eval "$(pyenv virtualenv-init -)"; fi

# Node
if which nodenv > /dev/null; then eval "$(nodenv init -)"; fi

# Haskell
path=($HOME/.cabal/bin(N-/) $path)

# Google Cloud SDK
path=($HOME/.local/share/google-cloud-sdk/bin(N-/) $path)

# Go
export GOPATH=$HOME/.go
path=($GOPATH/bin(N-/) $path)

# Local
path=($HOME/.local/bin(N-/) $path)
manpath=($HOME/.local/share/man(N-/) $manpath)

export EDITOR=vi
if which lv > /dev/null; then
    export PAGER=lv
else
    export PAGER=less
fi

# tar cf 時に ._ ファイルを含めない
export COPYFILE_DISABLE=true

ulimit -c 0

umask 022

[ -f /Applications/Emacs.app/Contents/MacOS/Emacs ] &&
export EMACS=/Applications/Emacs.app/Contents/MacOS/Emacs
