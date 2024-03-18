/*
** EPITECH PROJECT, 2023
** my_compute_square_root
** File description:
** return square root of a number
*/

#include "my.h"

int calc(int nb, int i)
{
    while (i <= 46340 && i * i != nb) {
        i++;
    }
    if (nb == i * i) {
        return i;
    } else {
        return 0;
    }
}

int my_compute_square_root(int nb)
{
    int i = 1;
    int val;

    if (nb > 0) {
        val = calc(nb, i);
        return val;
    } else {
        return 0;
    }
}
