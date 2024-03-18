##
## EPITECH PROJECT, 2023
## Makefile
## File description:
## make my_printf
##

SRC	=	my_printf.c			\
		hashtag.c			\
		my_len_nbr.c			\
		my_putstr_len.c			\
		my_put_unsigned_nbr.c		\
		decimal_into_binary.c		\
		decimal_into_hexadecimal_min.c	\
		decimal_into_hexadecimal_up.c	\
		decimal_into_octal.c 		\
		my_putchar_percent.c 		\
		my_char_isnum.c			\
		my_str_to_array_printf.c	\
		my_put_float.c			\
		pointer_into_hexadecimal.c	\
		exponent_min.c			\
		exponent_max.c			\
		my_put_float_prec.c		\
		./lib/my/my_compute_power_rec.c \
		./lib/my/my_compute_square_root.c \
		./lib/my/my_find_prime_sup.c \
		./lib/my/my_getnbr.c \
		./lib/my/my_isneg.c \
		./lib/my/my_is_prime.c \
		./lib/my/my_putchar.c \
		./lib/my/my_put_nbr.c \
		./lib/my/my_putstr.c \
		./lib/my/my_revstr.c \
		./lib/my/my_showmem.c \
		./lib/my/my_sort_int_array.c \
		./lib/my/my_strcapitalize.c \
		./lib/my/my_strcat.c \
		./lib/my/my_strcmp.c \
		./lib/my/my_strcpy.c \
		./lib/my/my_str_isalpha.c \
		./lib/my/my_str_islower.c \
		./lib/my/my_str_isnum.c \
		./lib/my/my_str_isprintable.c \
		./lib/my/my_str_isupper.c \
		./lib/my/my_strlen.c \
		./lib/my/my_strlowcase.c \
		./lib/my/my_strncat.c \
		./lib/my/my_strncmp.c \
		./lib/my/my_strncpy.c \
		./lib/my/my_strstr.c \
		./lib/my/my_strupcase.c \
		./lib/my/my_swap.c \
		./lib/my/my_strdup.c \
		./lib/my/my_showstr.c \

OBJ	=	$(SRC:.c=.o)

NAME	=	my_printf

all:	$(NAME)


$(NAME):	$(OBJ)
	ar rc libmy.a $(OBJ)

clean:
	rm -f $(OBJ)

fclean: clean
	rm -f $(NAME)

re:	fclean all
