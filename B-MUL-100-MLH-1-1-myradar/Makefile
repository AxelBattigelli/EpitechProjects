##
## EPITECH PROJECT, 2024
## my_radar
## File description:
## Project root Makefile
##

CC 		= 	gcc

MAIN		=	$(addprefix $(SRC_D), $(MAIN_F))
OBJ_M		=	$(MAIN:.c=.o)
MAIN_F		=	main.c

SRC		= 	$(addprefix $(SRC_D), $(SRC_F))
OBJ 		= 	$(SRC:.c=.o)
SRC_D 		= 	src/
SRC_F 		=	usage.c \
			simulation.c \
			draw/draw_plane.c \
			draw/draw_towers.c \
			draw/draw_background.c \
			draw/draw_timer.c \
			events/events.c \
			utilities/geometry.c \
			utilities/positions_checking.c \
			utilities/get_box_corners.c \
			collisions/box_collisions.c \
			collisions/plane_collisions.c \
			structures/tower.c \
			structures/window.c \
			structures/path.c \
			structures/plane/plane_create.c \
			structures/plane/plane_init.c \
			structures/plane/plane_move.c \
			structures/sim/sim.c \
			structures/sim/sim_fonts.c \
			structures/sim/sim_textures.c \
			structures/sim/sim_texts.c \
			structures/sim/sim_states.c \
			file_manipulation/get_file_buffer.c \
			file_manipulation/get_entities_from_file.c \

INC	=	-I./include/ -I./include/structures/ -I./include/structures/sim/

# CFLAGS	= 	-W -Wall -Wextra -Werror
CFLAGS		=	-g $(INC) $(LDFLAGS)

DBFLAGS		=	-g -g3 -ggdb

LDFLAGS 	=	-L./lib -lmy -lcsfml-graphics -lcsfml-system -lm

NAME 		= 	my_radar

all: $(NAME)

$(NAME):	makelib $(OBJ) $(OBJ_M)
	$(CC) -o $(NAME) $(OBJ) $(OBJ_M) $(CFLAGS)

makelib:
	make -C ./lib/my/ all

clean:
	rm -f $(OBJ)
	rm -f *.o*
	make -C ./lib/my clean

fclean: clean
	rm -f $(NAME)
	rm -f $(NAME_UT)
	make -C ./lib/my clean

re: fclean all
