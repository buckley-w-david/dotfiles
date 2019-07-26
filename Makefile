.PHONY: .vimrc .bashrc .tmux.conf .editorconfig xmonad.hs .xmobarrc

all: .vimrc .bashrc .tmux.conf .editorconfig xmonad.hs .xmobarrc

DIR = $(dir $(realpath $(firstword $(MAKEFILE_LIST))))

.vimrc:
	ln -sfn "$(DIR)/.vimrc" ~/.vimrc
	rm -rf ~/.vim/bundle/Vundle.vim
	git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
	vim +PluginInstall +qall

.bashrc:
	ln -sfn "$(DIR)/.bashrc.common" ~/.bashrc.common
	echo "source .bashrc.common" >> ~/.bashrc

.tmux.conf:
	ln -sfn "$(DIR)/.tmux.conf" ~/.tmux.conf

.editorconfig:
	ln -sfn "$(DIR)/.editorconfig" ~/.editorconfig

xmonad.hs:
	mkdir -p ~/.xmonad
	ln -sfn "$(DIR)/xmonad.hs" ~/.xmonad/xmonad.hs

.xmobarrc:
	ln -sfn "$(DIR)/.xmobarrc" ~/.xmobarrc
	mkdir -p ~/bin
	ln -sfn "$(DIR)/rss.sh" ~/bin/rss.sh
	ln -sfn "$(DIR)/screensaver.sh" ~/bin/screensaver.sh
	ln -sfn "$(DIR)/updates.sh" ~/bin/updates.sh
