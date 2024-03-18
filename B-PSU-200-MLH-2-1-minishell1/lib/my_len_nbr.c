/*
** EPITECH PROJECT, 2023
** my_len_nbr
** File description:
** return the len of a number
*/

#include "./include/my.h"

int my_len_nbr(int nb)
{
    int len = 0;
    int diviseur = 1;

    my_put_nbr(nb);
    if (nb == 0)
        return 1;
    for (int i = 0; nb / diviseur != 0; diviseur *= 10) {
        len++;
    }
    if (len > 10)
        return 10;
    return len;
}
