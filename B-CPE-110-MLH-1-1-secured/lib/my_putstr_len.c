/*
** EPITECH PROJECT, 2023
** my_putstr_len
** File description:
** display the string AND return his len
*/

#include "./include/my.h"

int my_putstr_len(char *str)
{
    int i = 0;

    while (str[i] != 0) {
        my_putchar(str[i]);
        i++;
    }
    return i;
}
