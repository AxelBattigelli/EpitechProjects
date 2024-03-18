/*
** EPITECH PROJECT, 2023
** my
** File description:
** contains all the prototypes of libmy
*/

#ifndef MY_H_
    #define MY_H_

int my_putchar(char c);
int my_put_nbr(int nb);
int my_putstr(char const *str);
__attribute__((const))
int my_strlen(char const *str);
int my_showstr(char const *str);
char *my_strdup(char const *src);

int my_printf(const char *format, ...);
int decimal_into_binary(int nb);
int decimal_into_hexadecimal_min(long divide);
int decimal_into_hexadecimal_up(int nb);
int decimal_into_octal(int nb);
int my_len_nbr(int nb);
int my_put_unsigned_nbr(unsigned int nb);
int my_putstr_len(char *str);
int hashtag(const char *format);
int my_putchar_percent(void);
int my_char_isnum(char c);
int my_putchar_r(char c);
char **my_str_to_array_printf(char const *str);
int my_put_float(double nb);
int pointer_into_hexadecimal(long *nb);
int exponent_max(double nb);
int exponent_min(double nb);
int my_put_float_prec(double nb, int prec);
int my_printf(const char *format, ...);

#endif /* MY_H_ */
