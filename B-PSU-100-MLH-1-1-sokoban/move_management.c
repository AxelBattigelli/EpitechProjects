/*
** EPITECH PROJECT, 2023
** move_management
** File description:
** player move management
*/

#include "include/my_sokoban.h"

static void left_management(char **map, position_t *pos, char **save_map)
{
    if (map[pos->pos_x][pos->pos_y - 1] != '#'
        && map[pos->pos_x][pos->pos_y - 1] != 'X') {
        map[pos->pos_x][pos->pos_y - 1] = 'P';
        map[pos->pos_x][pos->pos_y] = ' ';
    }
    if (map[pos->pos_x][pos->pos_y - 1] == 'X'
        && map[pos->pos_x][pos->pos_y - 2] != '#' &&
        map[pos->pos_x][pos->pos_y - 2] != 'X') {
        map[pos->pos_x][pos->pos_y - 2] = 'X';
        map[pos->pos_x][pos->pos_y - 1] = 'P';
        map[pos->pos_x][pos->pos_y] = ' ';
    }
    if (save_map[pos->pos_x][pos->pos_y] == 'O'
        && map[pos->pos_x][pos->pos_y - 1] != '#' &&
        map[pos->pos_x][pos->pos_y - 1] != 'X')
        map[pos->pos_x][pos->pos_y] = 'O';
}

static void right_management(char **map, position_t *pos, char **save_map)
{
    if (map[pos->pos_x][pos->pos_y + 1] != '#'
        && map[pos->pos_x][pos->pos_y + 1] != 'X') {
        map[pos->pos_x][pos->pos_y + 1] = 'P';
        map[pos->pos_x][pos->pos_y] = ' ';
    }
    if (map[pos->pos_x][pos->pos_y + 1] == 'X'
        && map[pos->pos_x][pos->pos_y + 2] != '#' &&
        map[pos->pos_x][pos->pos_y + 2] != 'X') {
        map[pos->pos_x][pos->pos_y + 2] = 'X';
        map[pos->pos_x][pos->pos_y + 1] = 'P';
        map[pos->pos_x][pos->pos_y] = ' ';
    }
    if (save_map[pos->pos_x][pos->pos_y] == 'O'
        && map[pos->pos_x][pos->pos_y + 1] != '#' &&
        map[pos->pos_x][pos->pos_y + 1] != 'X')
        map[pos->pos_x][pos->pos_y] = 'O';
}

static void down_management(char **map, position_t *pos, char **save_map)
{
    if (map[pos->pos_x + 1][pos->pos_y] != '#'
        && map[pos->pos_x + 1][pos->pos_y] != 'X') {
        map[pos->pos_x + 1][pos->pos_y] = 'P';
        map[pos->pos_x][pos->pos_y] = ' ';
    }
    if (map[pos->pos_x + 1][pos->pos_y] == 'X'
        && map[pos->pos_x + 2][pos->pos_y] != '#' &&
        map[pos->pos_x + 2][pos->pos_y] != 'X') {
        map[pos->pos_x + 2][pos->pos_y] = 'X';
        map[pos->pos_x + 1][pos->pos_y] = 'P';
        map[pos->pos_x][pos->pos_y] = ' ';
    }
    if (save_map[pos->pos_x][pos->pos_y] == 'O'
        && map[pos->pos_x + 1][pos->pos_y] != '#' &&
        map[pos->pos_x + 1][pos->pos_y] != 'X')
        map[pos->pos_x][pos->pos_y] = 'O';
}

static void up_management(char **map, position_t *pos, char **save_map)
{
    if (map[pos->pos_x - 1][pos->pos_y] != '#'
        && map[pos->pos_x - 1][pos->pos_y] != 'X') {
        map[pos->pos_x - 1][pos->pos_y] = 'P';
        map[pos->pos_x][pos->pos_y] = ' ';
    }
    if (map[pos->pos_x - 1][pos->pos_y] == 'X'
        && map[pos->pos_x - 2][pos->pos_y] != '#' &&
        map[pos->pos_x - 2][pos->pos_y] != 'X') {
        map[pos->pos_x - 2][pos->pos_y] = 'X';
        map[pos->pos_x - 1][pos->pos_y] = 'P';
        map[pos->pos_x][pos->pos_y] = ' ';
    }
    if (save_map[pos->pos_x][pos->pos_y] == 'O'
        && map[pos->pos_x - 1][pos->pos_y] != '#' &&
        map[pos->pos_x - 1][pos->pos_y] != 'X')
        map[pos->pos_x][pos->pos_y] = 'O';
}

static void conditionnal_test(char **map, position_t *pos, int i, int j)
{
    if (map[i][j] == 'P') {
        pos->pos_x = i;
        pos->pos_y = j;
    }
}

char **move_left(char **map, int nb_line, char **save_map)
{
    position_t *pos = malloc(sizeof(position_t *));

    for (int i = 0; i != nb_line; i++)
        for (int j = 0; map[i][j] != '\0'; j++)
            conditionnal_test(map, pos, i, j);
    left_management(map, pos, save_map);
    free(pos);
    return map;
}

char **move_right(char **map, int nb_line, char **save_map)
{
    position_t *pos = malloc(sizeof(position_t *));

    for (int i = 0; i != nb_line; i++)
        for (int j = 0; map[i][j] != '\0'; j++)
            conditionnal_test(map, pos, i, j);
    right_management(map, pos, save_map);
    free(pos);
    return map;
}

char **move_down(char **map, int nb_line, char **save_map)
{
    position_t *pos = malloc(sizeof(position_t *));

    for (int i = 0; i != nb_line; i++)
        for (int j = 0; map[i][j] != '\0'; j++)
            conditionnal_test(map, pos, i, j);
    down_management(map, pos, save_map);
    free(pos);
    return map;
}

char **move_up(char **map, int nb_line, char **save_map)
{
    position_t *pos = malloc(sizeof(position_t *));

    for (int i = 0; i != nb_line; i++)
        for (int j = 0; map[i][j] != '\0'; j++)
            conditionnal_test(map, pos, i, j);
    up_management(map, pos, save_map);
    free(pos);
    return map;
}

char **move_management(int input_key, char **map, int nb_line, char **save_map)
{
    if (input_key == 65)
        map = move_up(map, nb_line, save_map);
    if (input_key == 66)
        map = move_down(map, nb_line, save_map);
    if (input_key == 67)
        map = move_right(map, nb_line, save_map);
    if (input_key == 68)
        map = move_left(map, nb_line, save_map);
    return map;
}
