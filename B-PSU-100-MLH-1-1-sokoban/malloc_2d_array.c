/*
** EPITECH PROJECT, 2023
** malloc_2d_array
** File description:
** malloc 2d array
*/

#include "include/my_sokoban.h"

char **fill_tab(char **tab, char *buffer)
{
    int i = 0;
    int j = 0;
    int index = 0;

    while (buffer[index] != '\0') {
        if (buffer[index] == '\n') {
            tab[i][j] = '\0';
            i++;
            j = 0;
        } else {
            tab[i][j] = buffer[index];
            j++;
        }
        index++;
    }
    return tab;
}

char **malloc_2d_array(char *buffer)
{
    char **array;
    int line = get_nb_line(buffer);
    int col = get_nb_col(buffer);
    int i = 0;

    array = malloc(sizeof(char *) * (line + 1));
    for (i = 0; i != line; i++)
        array[i] = malloc(sizeof(char) * (col + 1));
    array[i] = NULL;
    return array;
}
