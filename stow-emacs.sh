mkdir -p ~/crafted-dotfiles/emacs/custom-modules
mkdir -p ~/crafted-dotfiles/cli/shell
mkdir -p ~/crafted-dotfiles/alacritty
mkdir -p ~/crafted-dotfiles/emacs/.cache

cd ~/sync/emacs/
stow crafted-dotfiles -t ~/crafted-dotfiles

cd ~/crafted-dotfiles/emacs/
ln -s ~/sync/emacs/crafted-emacs
