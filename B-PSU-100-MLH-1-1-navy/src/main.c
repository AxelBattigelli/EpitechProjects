/*
** EPITECH PROJECT, 2024
** my_navy
** File description:
** my_navy
*/

#include "../include/my.h"
#include "../include/navy.h"

struct win_s win;

static int display_help(void)
{
    my_printf("USAGE\n");
    my_printf("     ./navy [first_player_pid] navy_positions\n");
    my_printf("DESCRIPTION\n");
    my_printf("     first_player_pid: only for the 2nd player.");
    my_printf(" pid of the first player.\n");
    my_printf("     navy_positions file representing the positions");
    my_printf(" of the ships.\n");
    return 0;
}

int main(int ac, char **av)
{
    int fd;
    char buffer[1];

    win.win = 10;
    if (ac == 2)
        if (av[1][0] == '-' && av[1][1] == 'h' && av[1][2] == '\0')
            return display_help();
    if (ac == 1 || ac > 3) {
        my_putstr("navy: Invalid options, try -h for see help\n");
        return (84);
    }
    navy(ac, av, -1);
    if (win.win == 1) {
        my_putstr("Enemy won\n");
        return 1;
    } else if (win.win == 0) {
        my_putstr("I won\n");
        return 0;
    }
    return (84);
}
