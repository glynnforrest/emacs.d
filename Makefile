.PHONY: help
help:
	@echo 'Usage: make [target]'
	@echo 'Available targets:'
	@echo
	@grep -Eo '^[-a-z/]+' Makefile | sort

.PHONY: prepare
prepare:
	if test ! -d ~/.config; then mkdir -v ~/.config; fi

.PHONY: link
link: prepare
	stow -v -t ~ bin
	stow -v -t ~ git
	stow -v -t ~ nvim
	stow -v -t ~ pgcli
	stow -v -t ~ tmux
	stow -v -t ~ vim
	stow -v -t ~ zsh

.PHONY: clean
clean:
	for i in $$(find . -type d -maxdepth 1); do stow -v -t ~ -D $$(basename $$i); done
