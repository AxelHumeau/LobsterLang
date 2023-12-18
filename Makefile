##
## EPITECH PROJECT, 2023
## GLaDOS
## File description:
## Makefile
##

NAME = glados

PKG_NAME = LobsterLang

RM = rm -f

all: $(NAME)

$(NAME):
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

.PHONY: all clean fclean re tests_run
