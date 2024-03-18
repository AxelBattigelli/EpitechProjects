/*
** EPITECH PROJECT, 2023
** my_str_islower
** File description:
** Returns 1 if the string contains only lowercase letters
*/

#include "my.h"

int my_str_islower(char const *str)
{
    int i = 0;

    while (str[i] != '\0') {
        if (!my_char_islower(str[i]))
            return 0;
        i++;
    }
    return 1;
}
