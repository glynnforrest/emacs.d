# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
# ZSH_THEME="robbyrussell"
ZSH_THEME="steeef"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how many often would you like to wait before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git history-substring-search)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
export PATH=/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/bin/core_perl:/opt/qt/bin

# My customisations
setopt autocd

#Common aliases
alias ls='ls -h --group-directories-first --color=always'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'

alias c='cd -'
alias e='emacsclient -n'

#mkdir a cd into it in one command
mkcd () {
	mkdir $1 -p && cd $1
}

#restart emacs server
er () {
	emacsclient -e '(my-kill-emacs)'; emacs -daemon
}

alias sz="source ~/.zshrc"
alias ez="e ~/.zshrc"

alias v='vim'
alias png='ping www.google.com -c 5'
alias get='packer'
alias bin='sudo pacman -Rs'

alias pup='sudo pacman -Syu'
alias porph='pacman -Qqdt' #Show package orphans
alias pgr='pacman -Qq | grep -i' #Grep installed packages

alias open="xdg-open"

#Git aliases
alias gst='git status'
alias gch='git checkout'
alias gco='git commit'
alias gad='git add'
alias gcl='git clone'
alias gbr='git branch'
alias glo='git log'
alias gloo='git log --oneline'
alias gfe='git fetch'
alias gpul='git pull'
alias gpus='git push'
alias gme='git merge'
alias gdi='git diff'
alias gsu='git submodule'

#Grep for a process
alias psgr='ps -A | grep -i'
alias mysq='mysql -u root -p'
alias starwars='telnet towel.blinkenlights.nl'

placeholder () {
	wget http://placekitten.com/$1/$2 -O $1\x$2.jpg
}

alias myip='curl canihazip.com/s/'
