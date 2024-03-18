##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## make navy
##

SRC	=	src/main.c \
		src/navy.c \
		src/where_is_boat.c \
	  	src/my_show_tab.c \
		src/client.c \
		src/my_putf.c \
		src/my_intostr.c \
		src/let_play.c \
		src/play.c \
		src/win_or_not.c \
		src/ennemy_tab.c \
		src/loop_sig.c \
		src/for_play.c \
		src/check_boat.c \
		src/load_map.c

OBJ = $(SRC:.c=.o)

LIBS = -L ./lib -lmy
FLAGS += -I ./include

NAME = my_navy


all: $(NAME)

$(NAME): $(OBJ)
	make -C lib/
	gcc -o $(NAME) -g $(OBJ) $(LIBS)

tests_run:
	$(CC) -Dmain=my_main -o unit-tests $(SRC) ./tests/tests.c $(FLAGS) $(LIBS)\
	--coverage -lcriterion $(LDFLAGS) -g
	./unit-tests

clean:
	rm -f src/*.o
	$(MAKE) -C lib/ clean

fclean: clean
	rm -f $(NAME)
	rm -f unit-tests*

re: fclean all
