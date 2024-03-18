##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## makefile for all final stumper
##

SRC	=	final_stumper.c	\
		line.c		\
		main.c		\
		rush1.c

OBJ	=	$(SRC:.c=.o)

NAME	=	rush3

LIB	=	-L./lib/my/ -lmy

INC	=	-I./include/

all:	$(NAME)
	make -C ./lib/my/
	gcc -o $(NAME) $(OBJ) $(INC) $(LIB)

$(NAME):	$(OBJ)

clean:
	make clean -C ./lib/my/
	rm -f $(OBJ)

fclean:	clean
	make fclean -C ./lib/my/
	rm -f $(NAME)

re: 	fclean all
