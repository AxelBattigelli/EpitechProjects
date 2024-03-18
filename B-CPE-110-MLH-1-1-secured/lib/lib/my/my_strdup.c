/*
** EPITECH PROJECT, 2023
** my_strdup
** File description:
** allocate memory and and copy the string into it
*/

#include "my.h"
#include <stdlib.h>

char *my_strdup(char const *src)
{
    int i = 0;
    char *str;
    int len_src = my_strlen(src);

    if (src[0] == 0)
        return 0;
    str = malloc(sizeof(char) * (len_src + 1));
    while (i < len_src) {
        str[i] = src[i];
        i++;
    }
    str[i] = '\0';
    return str;
}
