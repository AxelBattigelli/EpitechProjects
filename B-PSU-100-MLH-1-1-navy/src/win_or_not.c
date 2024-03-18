/*
** EPITECH PROJECT, 2024
** my_navy
** File description:
** my_navy
*/

#include "../include/my.h"
#include "../include/navy.h"

static int cond_win_or_not1(char *tab)
{
    if (*tab == 'x')
        return 1;
    return 0;
}

static int cond_win_or_not2(char *user)
{
    if (*user == 'x')
        return 1;
    return 0;
}

static int res_win(int x, int c)
{
    if (x == 14)
        win.win = 1;
    else if (c == 14)
        win.win = 0;
}

int win_or_not(char **tab, char **user1)
{
    int i = 0;
    int j;
    int x = 0;
    int c = 0;

    while (tab[i] != 0) {
        j = 0;
        while (tab[i][j] != '\0') {
            x += cond_win_or_not1(&tab[i][j]);
            c += cond_win_or_not1(&user1[i][j]);
            j++;
        }
        i++;
    }
}
