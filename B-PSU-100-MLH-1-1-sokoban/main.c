/*
** EPITECH PROJECT, 2023
** main
** File description:
** main of sokoban
*/

#include "include/my.h"
#include "include/my_sokoban.h"

char **create_map(char *buffer)
{
    char **map = malloc_2d_array(buffer);

    fill_tab(map, buffer);
    return map;
}

int end_game_case(char *buffer, char **map, char **saved_map)
{
    int nb_line = get_nb_line(buffer);
    int end = my_print(buffer, map, saved_map, nb_line);

    return end;
}

int game_loop(char **av)
{
    char *buffer = open_and_read_file(av[1]);
    char **map = create_map(buffer);
    char **saved_map = create_map(buffer);
    int end = end_game_case(buffer, map, saved_map);
    
    free(buffer);
    free(map);
    free(saved_map);
    return end;
}

int main(int ac, char **av)
{
    int res = 0;

    if (ac == 2 && my_strcmp(av[1], "-h") == 0)
        write(1, "USAGE\n\t./my_sokoban map\nDESCRIPTION\n\tmap file"
            "representing the warehouse map, containing ‘#’ for walls,\n\t‘P’"
        "for the player, ‘X’ for boxes and ‘O’ for storage locations.\n", 185);
    else if (error_management(ac, av) == 84)
        return 84;
    else
        res = game_loop(av);
    if (res == 0)
        write(1, "Victory !\n", 11);
    else
        write(1, "Defeat !\n", 10);
    return 0;
}
