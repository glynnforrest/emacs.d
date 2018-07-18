{% set user = 'glynn' %}

package_repos:
  file.managed:
    - name: /etc/apt/sources.list
    - source: salt://files/debian_stretch/sources.list
    - require_in:
        pkg: packages

packages:
  pkg.latest:
    - refresh: True
    - names:
      - apt-transport-https
      - chromium
      - curl
      - emacs25
      - figlet
      - firmware-iwlwifi
      - g++
      - git
      - htop
      - imagemagick
      - jq
      - make
      - markdown
      - stow
      - sudo
      - tig
      - tmux
      - tree
      - vim
      - watch
      - zsh

user_account:
  user.present:
    - name: {{user}}
    - shell: /usr/bin/zsh
    - createhome: true
    - optional_groups:
        - sudo
  require:
    - pkg: packages

docker:
  pkgrepo.managed:
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
  pkg.installed:
    - names:
      - exuberant-ctags
      - libncurses5-dev
  archive.extracted:
    - name: /tmp/global
    - source: {{global_src}}
    - source_hash: {{global_hash}}
    - unless: which global
  cmd.run:
    - name: './configure --with-exuberant-ctags=/usr/bin/ctags-exuberant && make && make install'
    - cwd: '/tmp/global/global-6.6'
    - unless: which global

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
  cmd.run:
    - name: 'make install'
    - cwd: /tmp/fasd
    - unless: which fasd
    - require:
      - git: fasd

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
