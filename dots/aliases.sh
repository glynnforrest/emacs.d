alias e='emacsclient -n'
alias ec='emacsclient -nw'
alias ls='ls -h --group-directories-first --color=always'
alias ll='ls -l'
alias la='ls -a'
alias lla='ls -la'
alias c='cd -'
alias ..='cd ..'
alias ...='cd ../..'
alias grep='grep --color=auto'
alias png='ping www.google.com -c 5'
alias get='packer'
alias bin='sudo pacman -Rs'
alias pup='sudo pacman -Syu'
alias porph='pacman -Qqdt' #Show package orphans
alias pgr='pacman -Qq | grep -i' #Grep installed packages
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
alias psgr='ps -A | grep -i' #Grep for a process
alias mysq='mysql -u root -p'
alias starwars='telnet towel.blinkenlights.nl'
#Now load machine specific or private aliases
alias vx="vim /home/glynn/.xmonad/xmonad.hs"
alias re="xmonad --recompile"
alias youtube-mp3="youtube-dl -t --extract-audio --audio-format mp3 --audio-quality 320k"

#Zsh specific
alias zz="source ~/.zshrc"

#Bash specific
alias sb="source ~/.bashrc"
