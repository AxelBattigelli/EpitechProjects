##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## makefile for organized
##

SRC	=	main.c					\
		next_id_txt.c			\
		add.c					\
		del.c					\
		disp.c					\
		sort.c

OBJ	=	$(SRC:.c=.o)

NAME	=	organized

LIB	=	-L./lib -lshell -lmy

INC	=	-I./include

CFLAGS = -g

all:	$(NAME)

$(NAME):	$(OBJ)
	$(MAKE) -C lib
	$(CC) $(CFLAGS) -g -o $(NAME) $(OBJ) $(INC) $(LIB)

clean:
	$(MAKE) -C lib clean
	rm -f $(OBJ)

fclean:	clean
	$(MAKE) -C lib clean
	rm -f $(NAME)

tests_run:
	$(CC) -Dmain=my_main -o unit-tests $(SRC) ./tests/tests.c $(INC) $(LIB) \
	--coverage -lcriterion $(LDFLAGS) -g
	./unit-tests

re: 	fclean all
