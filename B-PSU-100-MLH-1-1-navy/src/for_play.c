/*
** EPITECH PROJECT, 2024
** my_navy
** File description:
** my_navy
*/

#include "../include/my.h"
#include "../include/navy.h"

char **for_play(char *str, int pid, struct sigaction *signal, char **user1)
{
    loop_sig(str, pid);
    signal->sa_sigaction = hit;
    sigaction(SIGUSR1, signal, NULL);
    signal->sa_sigaction = hit;
    sigaction(SIGUSR2, signal, NULL);
    pause();
    if (win.win == 4)
        my_putstr("hit\n\n");
    if (win.win == 5)
        my_putstr("missed\n\n");
    user1 = ennemy_tab(str, user1);
    return (user1);
}
