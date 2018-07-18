prepare:
	if test ! -d ~/.config; then mkdir -v ~/.config; fi
	./bin/.bin/install-deps.sh

base: prepare
	stow -v -t ~ bin
	stow -v -t ~ git
	stow -v -t ~ pgcli
	stow -v -t ~ tmux
	stow -v -t ~ vim
	stow -v -t ~ zsh

media:
	stow -v -t ~ vimpc

mac: base media

linux: base media
	stow -v -t ~ xorg
	stow -v -t ~ xmonad

clean:
	for i in $$(find . -type d -maxdepth 1); do stow -v -t ~ -D $$(basename $$i); done

.PHONY: provision
provision:
	salt-call --local -l info --file-root=./provision state.sls debian_stretch
