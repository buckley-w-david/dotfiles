.PHONY: .vim .bashrc

all: .vim .bashrc

.vim:
	link .vimrc ~/.vimrc
	git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
	vim +PluginInstall +qall

.bashrc:
	link .bashrc.common ~/.bashrc.extend
	echo "source .bashrc.extend" >> ~/.bashrc
