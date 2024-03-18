/*
** EPITECH PROJECT, 2023
** sci
** File description:
** display exponent
*/

#include "./include/my.h"
#include "./include/my_printf.h"

int display_exp_m(double nb, int expo, int prec)
{
    int len = 0;

    len += my_put_float_prec(nb, prec);
    len += my_putchar_r('E');
    if (expo < 0) {
        expo *= -1;
        len += my_putchar_r('-');
    } else
        len += my_putchar_r('+');
    if (expo < 10)
        len += my_putchar_r('0');
        len += my_put_nbr(expo);
    return len;
}

int exponent_max(double nb)
{
    int len = 0;
    int expo = 0;

    if (nb < 0) {
        my_putchar('-');
        nb *= -1;
        len++;
    }
    while (nb >= 10 || nb < 1) {
        if (nb >= 10) {
            nb /= 10;
            expo++;
        }
        if (nb < 1) {
            nb *= 10;
            expo--;
        }
    }
    len += display_exp_m(nb, expo, 6);
    return len;
}
