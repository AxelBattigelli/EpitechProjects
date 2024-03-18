##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## make mysh
##

SRC	=	src/main.c \
		src/my_str_to_word_array_custom.c	\
		src/my_exit.c	\
		src/my_setenv.c	\
		src/my_unsetenv.c	\
		src/disp_env.c	\
		src/my_cd.c	\
		src/exec_prog.c	\

OBJ = $(SRC:.c=.o)

LIBS = -L ./lib -lmy
CPPFLAGS += -I ./include
CFLAGS += -g -gdwarf-4 #-fsanitize=address,undefined
export CFLAGS

NAME = mysh

all: $(NAME)

$(NAME): $(OBJ)
	$(MAKE) -C lib/
	$(CC) $(CFLAGS) -o $(NAME) -g -gdwarf-4 $(OBJ) $(LIBS)

tests_run:
	$(CC) -Dmain=my_main -o unit-tests $(SRC) ./tests/tests.c $(CPPFLAGS) \
	$(LIBS) --coverage -lcriterion $(LDFLAGS) -g
	./unit-tests

clean:
	find -name '*.o' -delete
	$(MAKE) -C lib/ clean

fclean: clean
	rm -f $(NAME)
	rm -f unit-tests*

re: fclean all
