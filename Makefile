prepare:
	./bin/install-deps.sh

base: prepare
	stow -v -t ~ vim
	stow -v -t ~ zsh
	stow -v -t ~ tmux

linux: base
	stow -v -t ~ xorg
	stow -v -t ~ xmonad

clean:
	for i in $$(find . -type d -maxdepth 1); do stow -v -t ~ -D $$(basename $$i); done
