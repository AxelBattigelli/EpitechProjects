/*
** EPITECH PROJECT, 2024
** my_navy
** File description:
** my_navy
*/

#include "../include/my.h"
#include "../include/navy.h"



char **ennemy_tab(char *str, char **user1)
{
    char *list = "ABCDEFGH";
    int i = 0;
    int j = 2;

    while (str[0] != list[i]) {
        j = j + 2;
        i++;
    }
    i = 1;
    while (str[1] != 48) {
        i++;
        str[1] = str[1] - 1;
    }
    if (win.win == 4)
        user1[i][j] = 'x';
    else if (win.win == 5)
        user1[i][j] = 'o';
    return (user1);
}
