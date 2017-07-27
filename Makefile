prepare:
	./bin/install-deps.sh

base: prepare
	stow -v -t ~ vim
	stow -v -t ~ zsh
	stow -v -t ~ tmux
	mkdir -p ~/.bin
	stow -v -t ~/.bin bin

media:
	stow -v -t ~ vimpc

mac: base media

linux: base media
	stow -v -t ~ xorg
	stow -v -t ~ xmonad

clean:
	for i in $$(find . -type d -maxdepth 1); do stow -v -t ~ -D $$(basename $$i); done
