/*
** EPITECH PROJECT, 2023
** my_strcat
** File description:
** Concatenates two strings
*/

#include "my.h"

char *my_strcat(char *dest, char const *src)
{
    int len = my_strlen(dest);

    my_strcpy(&dest[len], src);
}
