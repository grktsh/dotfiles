fpath=(/usr/local/share/zsh/site-functions $fpath)

# Emacs キーバインドを使う
bindkey -e

HISTFILE=$HOME/.zsh_history
HISTSIZE=10000000
SAVEHIST=$HISTSIZE
# zshプロセス間でヒストリを共有する
setopt share_history
# ignore duplication command history list
setopt hist_ignore_dups
# zsh の開始, 終了時刻をヒストリファイルに書き込む
setopt extended_history
# すぐにヒストリファイルに追記する
setopt inc_append_history
# スペースで始まるコマンドラインはヒストリに追加しない
setopt hist_ignore_space

autoload history-search-end

# 補完機能の強化
autoload -U compinit
compinit
# --prefix=/usr などの = 以降も補完
setopt magic_equal_subst

# auto directory pushd that you can get dirs list by cd -[tab]
setopt auto_pushd
# 同じディレクトリを pushd しない
setopt pushd_ignore_dups

setopt autocd

# compacked complete list display
#
setopt list_packed

# ビープを鳴らさない
setopt nobeep

# no beep sound when complete list displayed
#
setopt nolistbeep
setopt complete_aliases # aliased ls needs if file/dir completions work

# set prompt
PROMPT="%{[33m%}%~%{[m%}$ "
PROMPT2="%{[33m%}%_%{[m%}$ "
SPROMPT="%{[33m%}%r is correct? [n,y,a,e]:%{[m%} "
[ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
    PROMPT="%{[32m%}%m%{[m%}:${PROMPT}"

# http://d.hatena.ne.jp/dayflower/20081031/1225428086
case "$TERM" in *screen*)
    local -a shorthost

    echo $TERMCAP | grep -q -i screen
    if [ $? -eq 0 ]; then
        shorthost=""
    else
        shorthost="${HOST%%.*}:"
    fi

    echo -ne "\ek$shorthost\e\\"

    preexec() {
        echo -ne "\ek${shorthost}($1)\e\\"
        echo -ne "\e_`dirs`\e\\"
    }

    precmd() {
        echo -ne "\ek${shorthost}$(basename $(pwd))\e\\"
    }
esac

alias rm='rm -i'
alias cp='cp -ip'
alias mv='mv -i'
alias scp='scp -p'

alias ls='ls -FG'
alias ll='ls -l'
alias la='ls -A'

alias df='df -h'
alias du='du -h'

alias sudo='LANG=C sudo'

# https://github.com/direnv/direnv/wiki/Tmux
# It is recommended to wrap tmux to avoid issues with environment loading.
if which direnv > /dev/null; then
    alias tmux='direnv exec / tmux';
fi

# added by travis gem
[ -f /Users/grktsh/.travis/travis.sh ] && source /Users/grktsh/.travis/travis.sh

# The next line updates PATH for the Google Cloud SDK.
if [ -f '$HOME/.local/share/google-cloud-sdk/path.zsh.inc' ]; then
    source '$HOME/.local/share/google-cloud-sdk/path.zsh.inc';
fi

# The next line enables shell command completion for gcloud.
if [ -f '$HOME/.local/share/google-cloud-sdk/completion.zsh.inc' ]; then
    source '$HOME/.local/share/google-cloud-sdk/completion.zsh.inc';
fi
