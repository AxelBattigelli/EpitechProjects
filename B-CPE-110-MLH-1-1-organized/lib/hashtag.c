/*
** EPITECH PROJECT, 2023
** hashtag
** File description:
** display o, ox, oX that correspond to the type a the return number
*/

#include "./include/my_printf.h"
#include <stdarg.h>
#include "./include/my.h"

int hashtag(const char *format)
{
    int i = 0;
    int len = 0;

    if (format[i + 1] == 'o') {
        my_putchar('0');
        len++;
    }
    if (format[i + 1] == 'x') {
        my_putstr("0x");
        len += 2;
    }
    if (format[i + 1] == 'X') {
        my_putstr("0X");
        len += 2;
    }
    return (len);
}
