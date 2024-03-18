/*
** EPITECH PROJECT, 2023
** my_strncpy
** File description:
** Copies n characters from a string into another
*/

#include "my.h"

char *my_strncpy(char *dest, char const *src, int n)
{
    int i = 0;
    int len = my_strlen(src);

    while (i < n) {
        if (i > len - 1)
            dest[i] = '\0';
        if (i < len)
            dest[i] = src[i];
        i++;
    }
    return dest;
}
