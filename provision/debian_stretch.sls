{% set user = 'glynn' %}
{% set flatpak_apps = [
'com.slack.Slack',
'com.spotify.Client',
] %}

package_repos:
  file.managed:
    - name: /etc/apt/sources.list
    - source: salt://files/debian_stretch/sources.list
    - require_in:
        pkg: packages

packages:
  pkg.installed:
    - refresh: True
    - names:
      - apt-transport-https
      - chromium
      - curl
      - emacs25
      - exuberant-ctags
      - figlet
      - firmware-iwlwifi
      - flatpak
      - g++
      - git
      - htop
      - hugo
      - imagemagick
      - jq
      - libncurses5-dev
      - make
      - markdown
      - netbeans
      - nfs-common
      - nfs-kernel-server
      - procps
      - python-pip
      - stow
      - sudo
      - tig
      - tmux
      - tree
      - vagrant
      - vim
      - vlc
      - youtube-dl
      - zsh

flatpak:
  cmd.run:
    - name: "flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo"
    - unless: flatpak remote-list | grep flathub

{% for app in flatpak_apps %}
flatpak_{{app}}:
  cmd.run:
    - name: 'flatpak install --assumeyes flathub {{app}}'
    - unless: flatpak list | grep {{app}}
{% endfor %}

docker:
  pkgrepo.managed:
    - file: /etc/apt/sources.list.d/docker.list
    - name: deb https://download.docker.com/linux/debian stretch stable
    - key_url: https://download.docker.com/linux/debian/gpg
  pkg.installed:
    - name: docker-ce
    - require:
        - pkgrepo: docker
  service.running:
    - name: docker
    - enable: True
    - require:
        - pkg: docker

virtualbox:
  pkgrepo.managed:
    - file: /etc/apt/sources.list.d/virtualbox.list
    - name: deb http://download.virtualbox.org/virtualbox/debian stretch contrib
    - key_url: https://www.virtualbox.org/download/oracle_vbox_2016.asc
  pkg.installed:
    - names:
        - virtualbox-5.0
        - net-tools
    - require:
      - pkgrepo: virtualbox

vagrant_virtualbox_plugin:
  cmd.run:
    - name: 'vagrant plugin install vagrant-vbguest'
    - unless: 'vagrant plugin list | grep vagrant-vbguest'

user_account:
  user.present:
    - name: {{user}}
    - shell: /usr/bin/zsh
    - createhome: true
    - optional_groups:
        - sudo
        - docker
  require:
    - pkg: packages
    - pkg: docker

docker-compose:
  pip.installed:
    - name: docker-compose

nodejs:
  pkgrepo.managed:
    - file: /etc/apt/sources.list.d/nodejs.list
    - name: deb https://deb.nodesource.com/node_10.x stretch main
    - key_url: https://deb.nodesource.com/gpgkey/nodesource.gpg.key
  pkg.installed:
    - name: nodejs
    - require:
      - pkgrepo: nodejs
  npm.installed:
    - require:
      - pkg: nodejs
    - pkgs:
      - ember-cli
      - prez
      - vue-cli
      - vue-language-server
      - yarn

# using unless: for pkg.installed with .deb sources to prevent downloading them again
# this speeds things up significantly on subsequent runs
keepassxc:
  pkg.installed:
    - name: keepassxc
    - sources:
      - keepassxc: https://github.com/magkopian/keepassxc-debian/releases/download/2.3.3/keepassxc_2.3.3-1_amd64_stable_stretch.deb
    - unless: 'dpkg --get-selections | grep keepassxc'

dbeaver:
  pkg.installed:
    - name: dbeaver-ce
    - sources:
      - dbeaver-ce: https://dbeaver.io/files/5.1.3/dbeaver-ce_5.1.3_amd64.deb
    - unless: "dpkg --get-selections | grep dbeaver-ce"

hugo:
  pkg.installed:
    - name: hugo
    - sources:
      - hugo: https://github.com/gohugoio/hugo/releases/download/v0.44/hugo_0.44_Linux-64bit.deb
    - unless: "dpkg --get-selections | grep hugo"

rg:
  archive.extracted:
    - name: /tmp/ripgrep
    - source: https://github.com/BurntSushi/ripgrep/releases/download/0.7.1/ripgrep-0.7.1-x86_64-unknown-linux-musl.tar.gz
    - source_hash: 'a82447270183deefed44cfeed29b43915b199c93'
    - enforce_toplevel: True
    - unless: 'which rg'
  file.managed:
    - name: /usr/local/bin/rg
    - source: /tmp/ripgrep/ripgrep-0.7.1-x86_64-unknown-linux-musl/rg
    - mode: 0755
    - unless: 'which rg'
    - require:
      - archive: rg

{% set global_src = 'https://ftp.gnu.org/pub/gnu/global/global-6.6.tar.gz' %}
{% set global_hash = '0965b4800686641a28f7b16bb733aa3345316dde' %}
global:
  archive.extracted:
    - name: /tmp/global
    - source: {{global_src}}
    - source_hash: {{global_hash}}
    - unless: which global
  # cwd check for a non-exsiting folder fails, using jinja if instead
  # state file has to be ran twice, once to extract the archive, then
  # again so jinja will see it when state file is rendered
  {%- if salt['file.directory_exists']('/tmp/global/global-6.6') %}
  cmd.run:
    - name: './configure --with-exuberant-ctags=/usr/bin/ctags-exuberant && make && make install'
    - cwd: '/tmp/global/global-6.6'
    - unless: which global
    - require:
      - archive: global
  {%- endif %}

fzf:
  archive.extracted:
    - name: /tmp/fzf
    - source: https://github.com/junegunn/fzf-bin/releases/download/0.17.3/fzf-0.17.3-linux_amd64.tgz
    - source_hash: '36a0cea94d2571729b0117ad51e3df8b99b8b86a'
    - enforce_toplevel: False
    - unless: which fzf
  file.managed:
    - name: /usr/local/bin/fzf
    - source: /tmp/fzf/fzf
    - mode: 0755
    - unless: which fzf
    - require:
      - archive: fzf

fasd:
  git.latest:
    - name: https://github.com/clvv/fasd.git
    - rev: master
    - target: /tmp/fasd
    - unless: which fasd
  {%- if salt['file.directory_exists']('/tmp/fasd') %}
  cmd.run:
    - name: 'make install'
    - cwd: /tmp/fasd
    - unless: which fasd
    - require:
      - git: fasd
  {%- endif %}

php:
  pkgrepo.managed:
    - file: /etc/apt/sources.list.d/php.list
    - name: 'deb https://packages.sury.org/php/ stretch main'
    - key_url: https://packages.sury.org/php/apt.gpg
  pkg.installed:
    - names:
      - 'php7.1'
      - 'php7.1-gd'
      - 'php7.1-mysql'
      - 'php7.1-pgsql'
      - 'php7.1-mcrypt'
      - 'php7.1-curl'
      - 'php7.1-sqlite3'
      - 'php7.1-intl'
      - 'php-imagick'
      - 'php7.1-fpm'
      - 'php7.1-xml'
      - 'php7.1-zip'
      - 'php7.1-mbstring'
    - require:
        - pkgrepo: php

php_cs_fixer:
  file.managed:
    - name: /usr/local/bin/php-cs-fixer
    - source: 'https://github.com/FriendsOfPHP/PHP-CS-Fixer/releases/download/v2.1.2/php-cs-fixer.phar'
    - source_hash: 'e42188019768b35067c11ef5ba653a41df5072ea'
    - mode: 0755

download_composer:
  cmd.run:
    - cwd: /tmp
    - name: 'curl -sS https://getcomposer.org/installer | php'
    - env:
        - COMPOSER_HOME: /usr/local/bin
    - unless: test -f /usr/local/bin/composer

install_composer:
  cmd.wait:
    - name: mv /tmp/composer.phar /usr/local/bin/composer
    - cwd: /root
    - unless: test -f /usr/local/bin/composer
    - watch:
      - cmd: download_composer

{% set ran_bin = 'ran_linux_amd64' %}
{% set ran_sha = 'b88f512f7cacf508b9290989be1b50da21b6dfbc' %}
{% set ran_src = 'https://github.com/m3ng9i/ran/releases/download/v0.1.3/' ~ ran_bin ~ '.zip' %}
ran_http_server:
  archive.extracted:
    - name: /tmp/ran-http
    - enforce_toplevel: False
    - source: {{ran_src}}
    - source_hash: {{ran_sha}}
    - unless: which ran
  file.managed:
    - name: /usr/local/bin/ran
    - source: /tmp/ran-http/{{ran_bin}}
    - mode: 0755
    - unless: which ran
    - require:
      - archive: ran_http_server

http_status_helper:
  file.managed:
    - name: /usr/local/bin/http
    - source: 'https://raw.githubusercontent.com/gazayas/http_bash/master/http'
    - source_hash: '63da0e676b5a1742e068fca40c25919b887d4db6'
    - mode: 0755
