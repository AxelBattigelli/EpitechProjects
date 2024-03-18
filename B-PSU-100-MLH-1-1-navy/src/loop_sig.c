/*
** EPITECH PROJECT, 2024
** my_navy
** File description:
** my_navy
*/

#include "../include/my.h"
#include "../include/navy.h"
#include <unistd.h>


void hole_2(int i, siginfo_t *sig, void *s)
{
    static int c = 0;

    if (i == SIGUSR1) {
        c++;
        win.win = c;
    } else if (i == SIGUSR2) {
        c = 0;
        win.win = 14;
    }
}

int loop_sig(char *str, int pid)
{
    int c = str[0] - 64;
    int nb = str[1] - 48;

    while (c != 0) {
        kill(pid, SIGUSR1);
        c--;
        usleep(100000);
    }
    kill(pid, SIGUSR2);
    usleep(100000);
    while (nb != 0) {
        kill(pid, SIGUSR1);
        nb--;
        usleep(100000);
    }
    kill(pid, SIGUSR2);
    return (1);
}

char *check_sig(void)
{
    char *str = malloc(sizeof(char) * 2);
    struct sigaction signal = {0};

    signal.sa_sigaction = hole_2;
    signal.sa_flags = SA_SIGINFO;
    win.win = 0;
    sigaction(SIGUSR1, &signal, NULL);
    sigaction(SIGUSR2, &signal, NULL);
    while (win.win != 14) {
        str[0] = win.win + 65;
        usleep(100000);
    }
    win.win = 0;
    while (win.win != 14) {
        str[1] = win.win + 48;
        usleep(100000);
    }
    str[2] = '\0';
    win.win = 10;
    return (str);
}
