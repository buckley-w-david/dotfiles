.PHONY: .vimrc .bashrc .tmux.conf

all: .vimrc .bashrc .tmux.conf .editorconfig

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
