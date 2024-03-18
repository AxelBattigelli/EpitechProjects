/*
** EPITECH PROJECT, 2023
** my_print
** File description:
** display map
*/

#include "include/my_sokoban.h"

static void init_screen(void)
{
    initscr();
    noecho();
}

static void end_screen(char *buffer, char **map)
{
    display(map, buffer);
    refresh();
    endwin();
}

int my_print(char *buffer, char **map, char **save_map, int nb_line)
{
    int input_key;
    int end_game = -1;

    init_screen();
    while (TRUE) {
        clear();
        display(map, buffer);
        input_key = getch();
        map = move_management(input_key, map, nb_line, save_map);
        if (input_key == 32)
            input_key = reset_game(map, save_map, nb_line);
        end_game = end_manager(buffer, map, save_map, nb_line);
        if (end_game == 1 || end_game == 0) {
            clear();
            refresh();
            break;
        }
    }
    end_screen(buffer, map);
    return end_game;
}
