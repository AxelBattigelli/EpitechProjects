##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## Makefile
##

SRC	=	my_print.c \
		malloc_2d_array.c \
		get_nb_line_and_col.c \
		error_handling.c \
		move_management.c \
		check_end_game_status.c \
		my_print_functions.c \
		main.c

OBJ	=	$(SRC:.c=.o)

NAME	=	my_sokoban

CC	=	gcc

LDFLAGS	=	-lmy -L ./lib/my/ -lncurses # -fsanitize=address,undefined

# CFLAGS	=	-g -W -Wall -Wextra -Werror -fsanitize=address,undefined

CPPFLAGS	=	-I ./include

all:	$(NAME)

$(NAME):	$(OBJ)
	make -C lib/my
	$(CC) -o $(NAME) $(OBJ) $(LDFLAGS)

clean:
	make -C lib/my clean
	rm -f $(OBJ)

fclean:	clean
	make -C lib/my fclean
	rm -f $(NAME)

tests_run:
	$(CC) -Dmain=my_main -o unit-tests $(SRC) ./tests/tests.c $(INC) $(LIB) \
	--coverage -lcriterion $(LDFLAGS) -g
	./unit-tests

re:	fclean	all
