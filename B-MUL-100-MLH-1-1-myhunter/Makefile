##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## compile the my_hunter
##

CPPFLAGS += -I ./include/

LDLIBS += -lcsfml-graphics -lcsfml-system -lcsfml-audio

# LDFLAGS += -fsanitize=undefined,address -g3
# CFLAGS += -fsanitize=undefined,address -g3

SRC	=	main.c				\
		my_strcmp.c			\
		my_putstr.c			\
		my_putchar.c		\
		structure.c			\
		events.c			\
		audio.c				\
		my_rand.c			\
		life_txt_gestion.c	\
		score_txt_gestion.c	\
		my_getnbr.c			\
		score_compare.c		\
		my_strdup.c

OBJ	=	$(SRC:.c=.o)

NAME 	=	my_hunter

all:	$(NAME)

$(NAME):	$(OBJ)
	$(CC) $(LDFLAGS) -o $(NAME) $(OBJ) $(LOADLIBES) $(LDLIBS)

clean:
	rm -f $(OBJ)

fclean:	clean
	rm -f $(NAME)

re:
	$(MAKE) fclean
	$(MAKE) all
