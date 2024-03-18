/*
** EPITECH PROJECT, 2024
** my_navy
** File description:
** my_navy
*/

#include "../include/my.h"
#include "../include/navy.h"

int count_chiff(int nb)
{
    int count = 10;
    int tmp = nb;
    int i = 0;

    while (nb > 10) {
        nb = tmp / count;
        count *= 10;
        i++;
    }
    if (nb > 0)
        i++;
    return (i);
}

char *my_intostr(int nb)
{
    int i = count_chiff(nb);
    char *str = malloc(sizeof(char) * i);
    int count = 1;

    while (i > 1) {
        i--;
        count *= 10;
    }
    i = 0;
    while (count >= 1) {
        str[i] = (nb / count) + 48;
        nb = nb % count;
        count /= 10;
        i++;
    }
    return (str);
}
