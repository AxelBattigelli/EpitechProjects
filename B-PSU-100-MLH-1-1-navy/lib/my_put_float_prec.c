/*
** EPITECH PROJECT, 2023
** my_put_float
** File description:
** display a float
*/

#include "./include/my.h"
#include "./include/my_printf.h"

static int end_nbr(int stack, int stack2)
{
    int len = 0;

    if (stack2 >= 5)
        len += my_len_nbr(stack + 1);
    else
        len += my_len_nbr(stack);
    return len;
}

static int decimal_part(double decimal, int prec)
{
    int len = 0;
    int stack = 0;
    double dec;
    int stack2 = 0;

    while (len < prec - 1) {
        decimal *= 10.0;
        stack = (int)decimal;
        my_put_nbr(stack);
        decimal -= stack;
        len++;
    }
    decimal *= 10.0;
    stack = (int)decimal;
    decimal -= stack;
    dec = decimal * 10.0;
    stack2 = (int)dec;
    len += end_nbr(stack, stack2);
    return len;
}

int my_put_float_prec(double nb, int prec)
{
    int len = 0;
    int integer;
    double decimal;

    if (nb < 0) {
        my_putchar('-');
        nb *= -1;
        len++;
    }
    integer = (int)nb;
    decimal = nb - integer;
    len += my_len_nbr(integer);
    my_putchar('.');
    len++;
    len += decimal_part(decimal, prec);
    return len;
}
