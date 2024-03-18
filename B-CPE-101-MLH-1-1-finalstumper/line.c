/*
** EPITECH PROJECT, 2023
** rush1
** File description:
** Functions for all assignements of rush1
*/

#include <stdlib.h>

char *line(int length, char beg, char mid, char end)
{
    char *str = malloc(sizeof(char) * (length + 1));

    if (length > 0)
        str[0] = beg;
    for (int i = 1; i < length - 1; i++)
        str[i] = mid;
    if (length > 1)
        str[length - 1] = end;
    str[length] = '\n';
    return str;
}
