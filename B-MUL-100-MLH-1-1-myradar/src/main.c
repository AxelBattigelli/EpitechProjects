/*
** EPITECH PROJECT, 2024
** main
** File description:
** Main file for my_radar
*/

#include <stdlib.h>
#include <time.h>
#include "my_radar.h"
#include "usage.h"
#include "events.h"
#include "sim.h"
#include "my.h"

int main(int ac, char **av)
{
    window_t *win = NULL;
    int exit_code = 0;

    exit_code = check_args(ac, av);
    if (exit_code == MY_EXIT_OPTION)
        return 0;
    else if (exit_code == 84)
        return (exit_code);
    srand(time(NULL));
    win = window_create();
    if (win == NULL) {
        my_puterr("my_radar: Couldn't create window\n");
        return 84;
    }
    exit_code = launch_simulation(win, av[1]);
    return 0;
}
