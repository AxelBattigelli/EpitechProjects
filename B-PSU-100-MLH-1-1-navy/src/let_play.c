/*
** EPITECH PROJECT, 2024
** my_navy
** File description:
** my_navy
*/

#include "../include/my.h"
#include "../include/navy.h"

void connect_usr1(int i, siginfo_t *sig, void *s)
{
    static int a = 0;

    if (a == 0) {
        my_putstr("\nsuccessfully connected\n\n");
        i++;
    }
}

static void connect_usr_get(int i, siginfo_t *sig, void *s)
{
    static int a = 0;

    if (a == 0) {
        i++;
    }
}

char **complete_sentence(char **tab, int *arr, int pid)
{
    if (tab[arr[1]][arr[0]] == 'x') {
        tab[arr[1]][arr[0]] = 'x';
        kill(pid, SIGUSR2);
        my_putstr("missed\n\n");
    } else if (tab[arr[1]][arr[0]] < 48 || tab[arr[1]][arr[0]] > 57) {
        tab[arr[1]][arr[0]] = 'o';
        kill(pid, SIGUSR2);
        my_putstr("missed\n\n");
    } else {
        tab[arr[1]][arr[0]] = 'x';
        kill(pid, SIGUSR1);
        my_putstr("hit\n\n");
    }
    return (tab);
}

char **know_case(int fd, char **tab, int pid, int *arr)
{
    char *str = check_sig();
    char *list = "ABCDEFGH";

    while (str[0] != list[arr[1]]) {
        arr[0] = arr[0] + 2;
        arr[1]++;
    }
    arr[1] = str[1] - 48 + 1;
    if (arr[0] < 0 || arr[0] > 16 || arr[1] < 0 || arr[1] > 9)
        return (NULL);
    my_putstr(str);
    my_putstr(":");
    tab = complete_sentence(tab, arr, pid);
    return (tab);
}

int loop_wait(int *who, char **tab, char **user1, struct sigaction *signal)
{
    int fd;
    int arr[2];

    arr[0] = 2;
    arr[1] = 0;
    my_putstr("\nwaiting for enemy's attack\n\nresult: ");
    sigaction(SIGUSR1, signal, NULL);
    pause();
    tab = know_case(-1, tab, who[0], arr);
    show_status(tab, user1);
    win_or_not(tab, user1);
    if (win.win == 0 || win.win == 1)
        return (win.win);
    play(who, tab, user1, signal);
}

static void disp_let_play(char **tab, char **user1)
{
    my_putstr("my navy:\n");
    my_show_tab(tab);
    my_putstr("\nenemy navy:\n");
    my_show_tab(user1);
}

static int let_play_return(int *who, char **tab, char **user1,
    struct sigaction *signal)
{
    int i = loop_wait(who, tab, user1, signal);

    if (i == 1 || i == 0)
        return (i);
}

static int *set_who(int *who, int pid)
{
    who = malloc(sizeof(int) * 3);
    who[0] = pid;
    who[1] = 1;
    return who;
}

int let_play(int pid, char **av, char **tab)
{
    struct sigaction signal = {0};
    char **user1 = dump_map();
    int *who = set_who(who, pid);

    signal.sa_sigaction = connect_usr1;
    signal.sa_flags = SA_SIGINFO;
    sigaction(SIGUSR1, &signal, NULL);
    pause();
    while (1) {
        if (win.win == 0 || win.win == 1)
            return (win.win);
        disp_let_play(tab, user1);
        signal.sa_sigaction = connect_usr_get;
        signal.sa_flags = SA_SIGINFO;
        sigaction(SIGUSR1, &signal, NULL);
        if ((let_play_return(who, tab, user1, &signal) == 1) ||
            (let_play_return(who, tab, user1, &signal) == 0))
            return let_play_return(who, tab, user1, &signal);
    }
}
