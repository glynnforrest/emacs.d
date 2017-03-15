prepare:
	./bin/install-deps.sh

base: prepare
	stow -v -t ~ vim
	stow -v -t ~ zsh
	stow -v -t ~ tmux

clean:
	stow -v -t ~ -D vim
	stow -v -t ~ -D zsh
	stow -v -t ~ -D tmux
