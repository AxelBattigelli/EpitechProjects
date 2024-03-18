/*
** EPITECH PROJECT, 2024
** my_navy
** File description:
** my_navy
*/

#include "../include/my.h"
#include "../include/navy.h"

void my_show_tab(char **tab)
{
    int i = 0;
    int j;

    while (tab[i] != 0) {
        my_putstr(tab[i]);
        my_putstr("\n");
        i++;
    }
}
