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

.PHONY: all clean fclean re tests_run
