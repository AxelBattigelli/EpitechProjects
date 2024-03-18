/*
** EPITECH PROJECT, 2023
** decimal into octal
** File description:
** convert decimal into octal
*/

#include <stdlib.h>
#include "./include/my.h"

int decimal_into_octal(int nb)
{
    char *str;
    int j;
    int len = 0;

    if (nb == 0)
        my_put_nbr(0);
    str = malloc(sizeof(char) * (11 + 1));
    for (int i = 0; nb != 0; i++) {
        str[i] = nb % 8;
        nb /= 8;
    }
    j = my_strlen(str) - 1;
    for (j; j != -1; j--) {
        len++;
        my_put_nbr(str[j]);
    }
    return len;
}
