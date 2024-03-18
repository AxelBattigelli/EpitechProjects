/*
** EPITECH PROJECT, 2023
** mini_printf
** File description:
** some parameters for printf
*/

#include <stdarg.h>

int len_nbr(int nb)
{
    int len = 0;

    while (nb > 0) {
        nb = nb / 10;
        len++;
    }
    return len;
}

int my_put_nbrlen(int nb)
{
    int len = 0;

    if (nb < 0) {
        my_putchar(45);
        nb = nb * -1;
        len++;
    }
    if (nb >= 0 && nb <= 9){
        my_putchar(nb + 48);
    }
    if (nb == -2147483648) {
        my_putstr("2147483648");
    } else if ( nb > 9) {
        my_put_nbrlen(nb / 10);
        my_putchar(nb % 10 + 48);
    }
    return len += len_nbr(nb);
}

int my_putstrlen(char const *str)
{
    int i = 0;
    int len = 0;

    while (str[i] != '\0') {
        my_putchar(str[i]);
        len++;
        i++;
    }
    return len;
}

int get_flag(char flag, va_list list)
{
    int length_print = 0;

    if (flag == 's')
        length_print = my_putstrlen(va_arg(list, char *));
    if (flag == 'c') {
        my_putchar(va_arg(list, int));
        length_print++;
    }
    if (flag == 'd')
        length_print = my_put_nbrlen(va_arg(list, int));
    if (flag == 'i')
        length_print = my_put_nbrlen(va_arg(list, int));
    if (flag == '%') {
        my_putchar('%');
        length_print++;
    }
    if (length_print > 0)
        return length_print;
    return -2147483647;
}

int mini_printf(const char *format, ...)
{
    va_list list;
    int i = 0;
    int length_print = 0;

    va_start(list, format);
    while (i < my_strlen(format)) {
        if (format[i] == '%') {
            length_print += get_flag(format[i + 1], list);
            i = i + 2;
        } else {
            my_putchar(format[i]);
            length_print++;
            i++;
        }
    }
    va_end(list);
    if (length_print == 0)
        return 0;
    return length_print;
}
