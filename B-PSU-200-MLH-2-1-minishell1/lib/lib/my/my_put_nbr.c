/*
** EPITECH PROJECT, 2023
** my_put_nbr
** File description:
** display a number
** (extrem negative can't be convert in a positive and print,
** it's display with other methode)
*/

#include "my.h"

int my_put_nbr(int nb)
{
    if (nb < 0) {
        my_putchar(45);
        nb = nb * -1;
    }
    if (nb >= 0 && nb <= 9){
        my_putchar(nb + 48);
    }
    if (nb == -2147483648) {
        my_putstr("2147483648");
    } else if (nb > 9) {
        my_put_nbr(nb / 10);
        my_putchar(nb % 10 + 48);
    }
    return 0;
}
