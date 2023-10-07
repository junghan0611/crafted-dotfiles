
RM=rm
CP=rsync -avP

install:
	$(CP) Makefile early-init.el init.el ~/emacs/crafted-dotfiles/emacs
	$(CP) custom-modules ~/emacs/crafted-dotfiles/emacs
#	$(CP) . ~/.config/emacs

clean:
	$(RM) -rf $(shell git ls-files -i --exclude-standard -o --directory)
