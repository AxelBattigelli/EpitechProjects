/*
** EPITECH PROJECT, 2023
** my_strcat
** File description:
** Concatenates two strings
*/

#include "my.h"

char *my_strncat(char *dest, char const *src, int n)
{
    int len = my_strlen(dest);

    my_strncpy(&dest[len], src, n);
}
