.PHONY: .vim .bashrc

all: .vim .bashrc

.vim:
	link .vimrc ~/.vimrc
	git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
	vim +PluginInstall +qall

.bashrc:
	link .bashrc ~/.bashrc_extend
	echo "source .bashrc_extend" >> ~/.bashrc
