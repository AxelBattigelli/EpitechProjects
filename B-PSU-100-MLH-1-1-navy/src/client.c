/*
** EPITECH PROJECT, 2024
** my_navy
** File description:
** my_navy
*/

#include "../include/my.h"
#include "../include/navy.h"



void aff_screen()
{
    my_putstr("my_pid:\t");
    my_put_nbr(getpid());
    my_putstr("\n\nwaiting for ennemy connexion...\n");
}

void connect(int i, siginfo_t *sig, void *s)
{
    win.win = sig->si_pid;
    my_putstr("\nenemy connected\n\n");
    kill(win.win, SIGUSR1);
}

int display_positions(char **t, char **u, struct sigaction *signal, int *w)
{
    while (1) {
        my_putstr("my navy:\n");
        my_show_tab(t);
        my_putstr("\nenemy navy:\n");
        my_show_tab(u);
        play(w, t, u, signal);
        if (win.win == 0 || win.win == 1)
            return (win.win);
    }
}

int client_one(char **av)
{
    struct sigaction signal = {0};
    char **tab = dump_map();
    char **user2 = dump_map();
    int *who;

    signal.sa_sigaction = connect;
    signal.sa_flags = SA_SIGINFO;
    tab = make_me_boats(av[1], tab);
    if (tab == NULL)
        return (-1);
    aff_screen();
    sigaction(SIGUSR2, &signal, NULL);
    pause();
    who = dump_who(who);
    return display_positions(tab, user2, &signal, who);
}

int client_two(char **av)
{
    char **tab = dump_map();
    int pid = my_getnbr(av[1]);

    if (pid == -1)
        return (-1);
    my_putstr("my_pid:\t");
    my_put_nbr(getpid());
    my_putstr("\n");
    tab = make_me_boats(av[2], tab);
    if (tab == NULL)
        return (-1);
    if (kill(pid, SIGUSR2) == -1)
        return (-1);
    else
        let_play(pid, av, tab);
    return (0);
}
