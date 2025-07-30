##
## EPITECH PROJECT, 2025
## wolfram
## File description:
## Makefile
##

LOCAL_INSTALL_ROOT	=	$(shell stack path --local-install-root)

NAME	=	wolfram

BIN_PATH	= $(shell echo $(LOCAL_INSTALL_ROOT)"/bin/"$(NAME)-exe)

all:
	stack build
	cp $(BIN_PATH) ./$(NAME)

clean:
	stack clean

fclean: clean
	stack purge
	rm -rf $(NAME)
	rm -rf coverage

re:
	${MAKE} fclean
	${MAKE} all

tests_run:
	stack test --coverage
	stack hpc report --all --destdir coverage

.PHONY = all re clean fclean
