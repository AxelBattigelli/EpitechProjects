/*
** EPITECH PROJECT, 2024
** my_navy
** File description:
** my_navy
*/

#include "../include/my.h"
#include "../include/navy.h"



int *dump_who(int *who)
{
    who = malloc(sizeof(int) * 2);
    who[0] = win.win;
    who[1] = 0;
    win.win = 10;
    return (who);
}

void hit(int i, siginfo_t *sig, void *s)
{
    if (i == SIGUSR1) {
        win.win = 4;
    }
    if (i == SIGUSR2) {
        win.win = 5;
    }
}

int check_coordinate(char *str, char **user1)
{
    int c;

    if (str == NULL || str[0] == '\0' || str[1] == '\0' || my_strlen(str) > 2
    || (str[0] - 65) < 0 || (str[0] - 65) >= 8 || str[1] <= 48
    || str[1] > 56) {
        my_putstr("\nwrong position\n");
        return (-1);
    }
    return (1);
}

void show_status(char **tab, char **user1)
{
    my_putstr("my navy:\n");
    my_show_tab(tab);
    my_putstr("\nenemy navy:\n");
    my_show_tab(user1);
}

static void display_play(char *str)
{
    my_putstr("\nresult: ");
    my_putstr(str);
    my_putstr(":");
}

static int win_win(int *who, char **tab, char **user1,
    struct sigaction *signal)
{
    if (win.win == 0 || win.win == 1)
        return (win.win);
    loop_wait(who, tab, user1, signal);
    if (win.win == 0 || win.win == 1)
        return (win.win);
}

int play(int *who, char **tab, char **user1, struct sigaction *signal)
{
    char *str = NULL;
    size_t str_sz = 0;
    ssize_t gl_ret = 0;

    win_or_not(tab, user1);
    if (win.win == 0 || win.win == 1)
        return (win.win);
    while (1) {
        my_putstr("\nattack:\t");
        gl_ret = getline(&str, &str_sz, stdin);
        str[gl_ret - 1] = '\0';
        if (check_coordinate(str, user1) == 1) {
            display_play(str);
            user1 = for_play(str, who[0], signal, user1);
            show_status(tab, user1);
            win_or_not(tab, user1);
            return win_win(who, tab, user1, signal);
        }
    }
}
