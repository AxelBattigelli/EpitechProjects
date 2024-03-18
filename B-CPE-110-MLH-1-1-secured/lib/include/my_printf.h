/*
** EPITECH PROJECT, 2023
** my_printf
** File description:
** .h of printf fcts
*/

#ifndef MY_PRINTF_H_
    #define MY_PRINTF_H_
    #include <stdarg.h>

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

#endif /* MY_PRINTF_H_ */
