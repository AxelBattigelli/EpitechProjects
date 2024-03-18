/*
** EPITECH PROJECT, 2024
** my_int_tostrnum
** File description:
** Converts an int to a string.
*/

#include <stdlib.h>
#include <stdbool.h>
#include <limits.h>
#include "my.h"

static char *my_int_to_strnum_second(int nb, int i)
{
    char *strnum = malloc(sizeof(char) * (my_intlen(nb) + 2));
    bool is_neg = (nb < 0) ? true : false;

    if (nb < 0) {
        if (nb == INT_MIN)
            return ("-2147483648");
        nb = -nb;
    } else if (nb == 0)
        return ("0");
    while (nb != 0) {
        strnum[i] = nb % 10 + '0';
        i++;
        nb = nb / 10;
    }
    if (is_neg) {
        strnum[i] = '-';
        i++;
    }
    strnum[i] = '\0';
    return (my_revstr(strnum));
}

char *my_int_to_strnum(int nb)
{
    return (my_int_to_strnum_second(nb, 0));
}
