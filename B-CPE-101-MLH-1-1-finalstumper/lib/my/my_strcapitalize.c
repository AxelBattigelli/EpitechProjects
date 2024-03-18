/*
** EPITECH PROJECT, 2023
** my_strcapitalize
** File description:
** Capitalize the first letter of each word
*/

#include <stdio.h>

static int is_lowercase(char c)
{
    return ('a' <= c && c <= 'z');
}

static int is_uppercase(char c)
{
    return ('A' <= c && c <= 'Z');
}

static int is_alphanumeric(char c)
{
    return is_lowercase(c) || is_uppercase(c) || ('0' <= c && c <= '9');
}

char *my_strcapitalize(char *str)
{
    int i = 0;

    if (str[0] == '\0')
        return str;
    else if (is_lowercase(str[0]))
        str[0] -= 32;
    while (str[i + 1] != '\0') {
        if (!is_alphanumeric(str[i]) && is_lowercase(str[i + 1]))
            str[i + 1] -= 32;
        if (is_alphanumeric(str[i]) && is_uppercase(str[i + 1]))
            str[i + 1] += 32;
        i++;
    }
    return str;
}
