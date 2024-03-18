/*
** EPITECH PROJECT, 2024
** my_puterr
** File description:
** print string to standard error.
*/

#include <unistd.h>

void my_puterr(char const *str)
{
    for (unsigned int i = 0; str[i]; i++)
        write(2, &(str[i]), 1);
}
