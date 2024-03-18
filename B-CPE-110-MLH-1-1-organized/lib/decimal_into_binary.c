/*
** EPITECH PROJECT, 2023
** decimal into binary
** File description:
** convert decimal into binary
*/

#include "./include/my.h"
#include <stdlib.h>

int decimal_into_binary(int nb)
{
    char *str;
    int j = -1;
    int len = 0;

    if (nb == 0)
        my_put_nbr(0);
    str = malloc(sizeof(char) * (31 + 1));
    for (int i = 0; nb != 0; i++) {
        str[i] = nb % 2;
        nb /= 2;
        j++;
    }
    for (j; j != -1; j--) {
        len++;
        my_put_nbr(str[j]);
    }
    return len;
}
