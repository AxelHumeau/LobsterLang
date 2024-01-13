##
## EPITECH PROJECT, 2023
## GLaDOS
## File description:
## Makefile
##

NAME = glados

PKG_NAME = LobsterLang

RM = rm -f

COV_PATH_FILE = cov_path

BROWSER = firefox

EXTENSION_NAME = lobsterlang-0.0.1.vsix

MODULE_PATH = extension/node_modules

all:
	cd $(PKG_NAME) && stack install $(PKG_NAME) --local-bin-path ..
	mv $(PKG_NAME)-exe $(NAME)

clean:
	cd $(PKG_NAME) && stack clean

fclean: clean
	cd $(PKG_NAME) && stack purge
	$(RM) $(NAME)

re: fclean all

tests_run:
	cd $(PKG_NAME) && stack test

cov:
	cd $(PKG_NAME) && stack test --coverage 2> >(tail -n 1 > $(COV_PATH_FILE))
	$(BROWSER) $$(cat $(PKG_NAME)/$(COV_PATH_FILE) | sed 's/.$$//')
	rm $(PKG_NAME)/$(COV_PATH_FILE)

install_extension: $(MODULE_PATH)
	cd extension && vsce pack && code --install-extension $(EXTENSION_NAME)

$(MODULE_PATH):
	cd extension && npm install

.PHONY: all clean fclean re tests_run
