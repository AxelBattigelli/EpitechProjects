/*
** EPITECH PROJECT, 2023
** check_end_game_status
** File description:
** check if end -> victory or defeat
*/

#include "include/my_sokoban.h"

static int get_nb_x_conditionnal(char **map, int i, int j)
{
    if (map[i][j] == 'X')
        return 1;
    return 0;
}

int get_nb_x(char **map, int nb_line)
{
    int nb_x = 0;

    for (int i = 0; i != nb_line; i++)
        for (int j = 0; map[i][j] != '\0'; j++)
            nb_x += get_nb_x_conditionnal(map, i, j);
    return nb_x;
}

static int get_nb_emplacement_conditionnal(char **save_map, int i, int j)
{
    if (save_map[i][j] == 'O')
        return 1;
    return 0;
}

int get_nb_emplacement(char **save_map, int nb_line)
{
    int nb_o = 0;

    for (int i = 0; i != nb_line; i++)
        for (int j = 0; save_map[i][j] != '\0'; j++)
            nb_o += get_nb_emplacement_conditionnal(save_map, i, j);
    return nb_o;
}

int check_stuck(char **map, int i, int j)
{
    if ((map[i][j - 1] == '#' || map[i][j - 1] == 'X') &&
        (map[i - 1][j] == '#' || map[i - 1][j] == 'X'))
        return 1;
    if ((map[i][j - 1] == '#' || map[i][j - 1] == 'X') &&
        (map[i + 1][j] == '#' || map[i + 1][j] == 'X'))
        return 1;
    if ((map[i][j + 1] == '#' || map[i][j + 1] == 'X') &&
        (map[i - 1][j] == '#' || map[i - 1][j] == 'X'))
        return 1;
    if ((map[i][j + 1] == '#' || map[i][j + 1] == 'X') &&
        (map[i + 1][j] == '#' || map[i + 1][j] == 'X'))
        return 1;
    return 0;
}

static int check_end_game_defeat_conditionnal(char **map, int i, int j)
{
    if (map[i][j] == 'X' && check_stuck(map, i, j) == 1)
        return 1;
    return 0;
}

int check_end_game_defeat(char *buffer, char **map)
{
    int nb_line = get_nb_line(buffer);
    int nb_x = get_nb_x(map, nb_line);
    int nb_x_stuck = 0;

    for (int i = 0; i != nb_line; i++)
        for (int j = 0; map[i][j] != '\0'; j++)
            nb_x_stuck += check_end_game_defeat_conditionnal(map, i, j);
    if (nb_x_stuck == nb_x)
        return 1;
    return 0;
}

static int check_end_game_victory_conditionnal(char **map, char **save_map,
    int i, int j)
{
    if (save_map[i][j] == 'O' && map[i][j] == 'X')
        return 1;
    return 0;
}

int check_end_game_victory(char **map, char **save_map, int nb_line)
{
    int cmt = 0;
    int nb_o = get_nb_emplacement(save_map, nb_line);

    for (int i = 0; i != nb_line; i++)
        for (int j = 0; save_map[i][j] != '\0'; j++)
            cmt += check_end_game_victory_conditionnal(map, save_map, i, j);
    if (cmt == nb_o)
        return 0;
    return 1;
}
