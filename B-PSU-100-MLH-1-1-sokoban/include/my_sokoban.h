/*
** EPITECH PROJECT, 2023
** my_sokoban
** File description:
** .h for sokoban
*/

#ifndef MY_SOKOBAN_H_
    #define MY_SOKOBAN_H_

    #include <curses.h>
    #include <unistd.h>
    #include <stdio.h>
    #include <unistd.h>
    #include <stdlib.h>
    #include <string.h>
    #include <sys/stat.h>
    #include <sys/types.h>
    #include <fcntl.h>

typedef struct position{
    int pos_x;
    int pos_y;
} position_t;

char *open_and_read_file(char const *filepath);
int get_nb_line(char *buffer);
int get_nb_col(char *buffer);
char **malloc_2d_array(char *buffer);
int my_print(char *buffer, char **map, char **save_map, int nb_line);
char **fill_tab(char **tab, char *buffer);
int get_nb_x(char **map, int nb_line);
int get_nb_emplacement(char **save_map, int nb_line);
int invalid_nb_box_empl(char *av);
int error_management(int ac, char **av);
char **move_left(char **map, int nb_line, char **save_map);
char **move_right(char **map, int nb_line, char **save_map);
char **move_up(char **map, int nb_line, char **save_map);
char **move_down(char **map, int nb_line, char **save_map);
char **move_management(int input, char **map, int nb_line, char **save_map);
int check_end_game_victory(char **map, char **save_map, int nb_line);
int check_end_game_defeat(char *buffer, char **map);
int end_manager(char *buffer, char **map, char **save_map, int nb_line);
int reset_game(char **map, char **save_map, int nb_line);
void display(char **map, char *buffer);

#endif /*MY_SOKOBAN_H_*/
