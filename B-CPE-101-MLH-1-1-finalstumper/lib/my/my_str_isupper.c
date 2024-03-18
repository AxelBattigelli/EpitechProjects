/*
** EPITECH PROJECT, 2023
** my_str_isupper
** File description:
** Returns 1 if the string passed contains only uppercase letters
*/

#include "my.h"

int my_str_isupper(char const *str)
{
    int i = 0;

    while (str[i] != '\0') {
        if (!my_char_isupper(str[i]))
            return 0;
        i++;
    }
    return 1;
}
