/*
** EPITECH PROJECT, 2023
** my_put_nbr
** File description:
** Prints an int
*/

#include "my.h"

int my_put_nbr(int nb)
{
    if (nb < 0) {
        my_putchar('-');
        my_put_nbr(-nb);
    } else if (nb < 10) {
        my_putchar('0' + nb);
    } else {
        my_put_nbr(nb / 10);
        my_putchar('0' + (nb % 10));
    }
}
