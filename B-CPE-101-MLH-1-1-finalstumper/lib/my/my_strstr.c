/*
** EPITECH PROJECT, 2023
** my_strstr
** File description:
** Searches for the first occurence of a substring in a string.
** If is found, returns a pointer to the substring,
** otherwise return a point to NULL
*/

#include <stddef.h>
#include "my.h"

char *my_strstr(char *str, char const *to_find)
{
    int beg = 0;
    int n = my_strlen(to_find);
    int cmp = 0;

    while (str[beg] != '\0') {
        cmp = my_strncmp(&str[beg], to_find, n);
        if (cmp == 0)
            return &str[beg];
        beg++;
    }
    return NULL;
}
