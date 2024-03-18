/*
** EPITECH PROJECT, 2023
** my_print_functions
** File description:
** display map
*/

#include "include/my_sokoban.h"

int end_manager(char *buffer, char **map, char **save_map, int nb_line)
{
    int end_game = -1;

    if (check_end_game_victory(map, save_map, nb_line) == 0)
        end_game = 0;
    if (check_end_game_defeat(buffer, map) == 1)
        end_game = 1;
    return end_game;
}

int reset_game(char **map, char **save_map, int nb_line)
{
    for (int i = 0; i != nb_line; i++)
        for (int j = 0; save_map[i][j] != '\n'; j++)
            map[i][j] = save_map[i][j];
    return 0;
}

void display(char **map, char *buffer)
{
    int nb_line = get_nb_line(buffer);

    for (int i = 0; i < nb_line; i++)
        mvprintw(i, 0, map[i]);
}
