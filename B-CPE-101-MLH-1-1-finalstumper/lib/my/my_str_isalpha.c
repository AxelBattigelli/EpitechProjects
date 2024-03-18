/*
** EPITECH PROJECT, 2023
** my_str_alpha
** File description:
** Returns 1 if the string is only alphabetical
*/

#include "my.h"

int my_str_isalpha(char const *str)
{
    int i = 0;

    while (str[i] != '\0') {
        if (!(my_char_islower(str[i]) || my_char_isupper(str[i])))
            return 0;
        i++;
    }
    return 1;
}
