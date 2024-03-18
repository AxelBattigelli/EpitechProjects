/*
** EPITECH PROJECT, 2023
** my_show_word_array
** File description:
** display each word per line
*/

#include <stddef.h>
#include "my.h"

int my_show_word_array(char *const *tab)
{
    int i = 0;

    while (tab[i] != NULL) {
        my_putstr(tab[i]);
        i++;
    }
    return 0;
}
