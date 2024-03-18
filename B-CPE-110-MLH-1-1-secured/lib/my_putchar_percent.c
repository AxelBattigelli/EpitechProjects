/*
** EPITECH PROJECT, 2023
** my putchar percent
** File description:
** putchar fonction for the percent variant
*/

#include <unistd.h>

int my_putchar_percent(void)
{
    char prct = 37;

    write(1, &prct, 1);
    return 1;
}
