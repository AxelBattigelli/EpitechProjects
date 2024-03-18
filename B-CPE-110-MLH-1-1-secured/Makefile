##
## EPITECH PROJECT, 2024
## secured
## File description:
## Project root Makefile
##

LIB			= 	$(addprefix $(LIB_D), $(LIB_F))
OBJ_L 		= 	$(LIB:.c=.o)
LIB_D 		= 	lib/
LIB_F 		=	my_printf.c						\
				hashtag.c						\
				my_len_nbr.c					\
				my_putstr_len.c					\
				my_put_unsigned_nbr.c			\
				decimal_into_binary.c			\
				decimal_into_hexadecimal_min.c	\
				decimal_into_hexadecimal_up.c	\
				decimal_into_octal.c 			\
				my_putchar_percent.c 			\
				my_char_isnum.c					\
				my_str_to_array_printf.c		\
				my_put_float.c					\
				pointer_into_hexadecimal.c		\
				exponent_min.c					\
				exponent_max.c					\
				my_put_float_prec.c				\
				lib/my/my_putchar.c 			\
				lib/my/my_put_nbr.c 			\
				lib/my/my_putstr.c 				\
				lib/my/my_strlen.c 				\
				lib/my/my_strdup.c 				\
				lib/my/my_showstr.c 			\

SRC			= 	$(addprefix $(SRC_D), $(SRC_F))
OBJ 		= 	$(SRC:.c=.o)
SRC_D 		= 	src/
SRC_F 		=	hash.c			\
				ht_delete.c 	\
				ht_dump.c		\
				ht_insert.c		\
				ht_search.c		\
				new_hashtable.c	\
				delete_hashtable.c

NAME 		= 	secured
LIBNAME		= 	libhashtable.a

all: $(NAME)

$(NAME):	$(OBJ_L) $(OBJ)
	$(AR) $(ARFLAGS) $(LIBNAME) $(OBJ_L) $(OBJ)

clean:
	rm -f $(OBJ_L)
	rm -f $(OBJ)
	rm -f *.o*

fclean: clean
	rm -f $(LIBNAME)

tests_run:
	$(CC) -o unit-tests $(SRC) ./tests/tests.c $(INC) $(LIB) \
	--coverage -lcriterion $(LDFLAGS) -g
	./unit-tests

re: fclean all
