/*
** EPITECH PROJECT, 2023
** my_compute_power_rec
** File description:
** return power of number with recursivity
*/

#include "my.h"

int my_compute_power_rec(int nb, int p)
{
    long nb2 = nb;

    if (p == 0) {
        return 1;
    } else if (p < 0) {
        return 0;
    } else {
        return nb2 * my_compute_power_rec(nb2, p - 1);
    }
}
