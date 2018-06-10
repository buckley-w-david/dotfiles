vim:
	link .vimrc ~/.vimrc
	git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
	vim +PluginInstall +qall

bash:
	link .bashrc ~/.bashrc_extend
	echo "source .bashrc_extend" >> ~/.bashrc

